{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Lib
    ( runZendeskMain
    , collectEmails
    , processTicket
    , processTickets
    , fetchTickets
    , listAndSortTickets
    ) where

import           Universum

import           Data.Attoparsec.Text.Lazy (eitherResult, parse)
import           Data.Text (isInfixOf, stripEnd)

import           CLI (CLI (..), getCliArgs)
import           DataSource (App, Attachment (..), AttachmentContent (..), Comment (..),
                             CommentBody (..), Config (..), IOLayer (..), TicketId (..),
                             TicketInfo (..), TicketStatus (..), TicketTag (..), TicketTags (..),
                             UserId (..), ZendeskLayer (..), ZendeskResponse (..), asksIOLayer,
                             asksZendeskLayer, assignToPath, defaultConfig, knowledgebasePath,
                             renderTicketStatus, runApp, tokenPath)
import           LogAnalysis.Classifier (extractErrorCodes, extractIssuesFromLogs,
                                         prettyFormatAnalysis, prettyFormatLogReadError,
                                         prettyFormatNoIssues)
import           LogAnalysis.KnowledgeCSVParser (parseKnowLedgeBase)
import           LogAnalysis.Types (ErrorCode (..), Knowledge, renderErrorCode, setupAnalysis)
import           Util (extractLogsFromZip)
import           Statistics (showStatistics)

------------------------------------------------------------
-- Functions
------------------------------------------------------------

runZendeskMain :: IO ()
runZendeskMain = do
    args <- getCliArgs
    putTextLn "Welcome to Zendesk classifier!"
    token <- readFile tokenPath  -- Zendesk token
    assignFile <- readFile assignToPath  -- Select assignee
    knowledges <- setupKnowledgebaseEnv knowledgebasePath
    assignTo <- case readEither assignFile of
        Right agentid -> return agentid
        Left  err     -> error err

    let cfg = defaultConfig
                   { cfgToken = stripEnd token
                   , cfgAssignTo = assignTo
                   , cfgAgentId = assignTo
                   , cfgKnowledgebase = knowledges
                   }

    -- At this point, the configuration is set up and there is no point in using a pure IO.
    case args of
        CollectEmails            -> runApp collectEmails cfg
        (ProcessTicket ticketId) -> void $ runApp (processTicket (TicketId ticketId)) cfg
        ProcessTickets           -> void $ runApp processTickets cfg
        FetchTickets             -> runApp fetchTickets cfg
        ShowStatistics           -> runApp (showStatistics getTickets) cfg


collectEmails :: App ()
collectEmails = do
    cfg <- ask

    let email   = cfgEmail cfg
    let userId  = UserId . fromIntegral $ cfgAgentId cfg

    -- We first fetch the function from the configuration
    listTickets <- asksZendeskLayer zlListAssignedTickets
    putTextLn $  "Classifier is going to extract emails requested by: " <> email
    tickets <- listTickets userId
    putTextLn $ "There are " <> show (length tickets) <> " tickets requested by this user."
    let ticketIds = foldr (\TicketInfo{..} acc -> tiId : acc) [] tickets
    mapM_ extractEmailAddress ticketIds


processTicket :: TicketId -> App [ZendeskResponse]
processTicket ticketId = do

    -- We first fetch the function from the configuration
    getTicketInfo       <- asksZendeskLayer zlGetTicketInfo
    printText           <- asksIOLayer iolPrintText

    printText "Processing single ticket"

    ticketInfoM         <- getTicketInfo ticketId

    -- TODO(ks): Better exception handling.
    let ticketInfo      = fromMaybe (error "Missing ticket info!") ticketInfoM

    attachments         <- getTicketAttachments ticketInfo

    zendeskResponse     <- mapM (inspectAttachment ticketInfo) attachments

    postTicketComment   <- asksZendeskLayer zlPostTicketComment
    _                   <- mapM postTicketComment zendeskResponse

    printText "Process finished, please see the following url"
    printText $ "https://iohk.zendesk.com/agent/tickets/" <> show ticketId

    pure zendeskResponse


processTickets :: App ()
processTickets = do
    sortedTicketIds     <- listAndSortTickets
    _                   <- mapM (processTicket . tiId) sortedTicketIds

    putTextLn "All the tickets has been processed."

getTickets :: App [TicketInfo]
getTickets = do
    Config{..}  <- ask

    let email   = cfgEmail
    let userId  = UserId . fromIntegral $ cfgAgentId

    -- We first fetch the function from the configuration
    listTickets <- asksZendeskLayer zlListAssignedTickets
    printText   <- asksIOLayer iolPrintText

    printText $ "Classifier is going to process tickets assign to: " <> email
    tickets     <- listTickets userId

    printText $ "There are " <> show (length tickets) <> " tickets."

    pure tickets

fetchTickets :: App ()
fetchTickets = do
    sortedTicketIds <- listAndSortTickets
    mapM_ (putTextLn . show) sortedTicketIds
    putTextLn "All the tickets has been processed."

listAndSortTickets :: App [TicketInfo]
listAndSortTickets = do

    Config{..}  <- ask

    let email   = cfgEmail
    let userId  = UserId . fromIntegral $ cfgAgentId

    -- We first fetch the function from the configuration
    listTickets <- asksZendeskLayer zlListAssignedTickets
    printText   <- asksIOLayer iolPrintText

    printText $ "Classifier is going to process tickets assign to: " <> email

    tickets     <- listTickets userId
    let filteredTicketIds = filterAnalyzedTickets tickets
    let sortedTicketIds   = sortBy compare filteredTicketIds

    printText $ "There are " <> show (length sortedTicketIds) <> " unanalyzed tickets."
    printText "Processing tickets, this may take hours to finish."

    pure sortedTicketIds


-- | Read CSV file and setup knowledge base
setupKnowledgebaseEnv :: FilePath -> IO [Knowledge]
setupKnowledgebaseEnv path = do
    kfile <- toLText <$> readFile path
    let kb = parse parseKnowLedgeBase kfile
    case eitherResult kb of
        Left e   -> error $ toText e
        Right ks -> return ks

-- | Collect email
extractEmailAddress :: TicketId -> App ()
extractEmailAddress ticketId = do
    -- Fetch the function from the configuration.
    getTicketComments <- asksZendeskLayer zlGetTicketComments

    comments <- getTicketComments ticketId
    let (CommentBody commentWithEmail) = cBody $ fromMaybe (error "No comment") (safeHead comments)
    let emailAddress = fromMaybe (error "No email") (safeHead $ lines commentWithEmail)
    liftIO $ guard ("@" `isInfixOf` emailAddress)
    liftIO $ appendFile "emailAddress.txt" (emailAddress <> "\n")
    liftIO $ putTextLn emailAddress


-- | Process specifig ticket id (can be used for testing) only inspects the one's with logs
-- TODO(ks): Switch to `(MonadReader Config m)`, pure function?
getTicketAttachments :: TicketInfo -> App [Attachment]
getTicketAttachments TicketInfo{..} = do

    -- Get the function from the configuration
    getTicketComments   <- asksZendeskLayer zlGetTicketComments
    comments            <- getTicketComments tiId

    -- However, if we want this to be more composable...
    pure $ getAttachmentsFromComment comments


-- | A pure function for fetching @Attachment@ from @Comment@.
getAttachmentsFromComment :: [Comment] -> [Attachment]
getAttachmentsFromComment comments = do
    -- Filter tickets without logs
    let commentsWithAttachments :: [Comment]
        commentsWithAttachments = filter commentHasAttachment comments

    -- Filter out ticket without logs
    let attachments :: [Attachment]
        attachments = concatMap cAttachments commentsWithAttachments

    -- Filter out non-logs
    filter isAttachmentZip attachments

  where
    commentHasAttachment :: Comment -> Bool
    commentHasAttachment comment = length (cAttachments comment) > 0

    -- Readability
    isAttachmentZip :: Attachment -> Bool
    isAttachmentZip attachment = "application/zip" == aContentType attachment


-- | Given number of file of inspect, knowledgebase and attachment,
-- analyze the logs and return the results.
--
-- The results are following:
--
-- __(comment, tags, bool of whether is should be public comment)__
inspectAttachment :: TicketInfo -> Attachment -> App ZendeskResponse
inspectAttachment ticketInfo@TicketInfo{..} att = do

    Config{..}      <- ask

    getAttachment   <- asksZendeskLayer zlGetAttachment
    printText       <- asksIOLayer iolPrintText

    attachment     <- fromMaybe (error "Missing Attachment content!") <$> getAttachment att

    let rawLog      = getAttachmentContent attachment
    let results     = extractLogsFromZip cfgNumOfLogsToAnalyze rawLog

    case results of
        Left _ -> do

            printText . renderErrorCode $ SentLogCorrupted

            pure ZendeskResponse
                { zrTicketId    = tiId
                , zrComment     = prettyFormatLogReadError ticketInfo
                , zrTags        = [renderErrorCode SentLogCorrupted]
                , zrIsPublic    = cfgIsCommentPublic
                }
        Right result -> do
            let analysisEnv             = setupAnalysis cfgKnowledgebase
            let eitherAnalysisResult    = extractIssuesFromLogs result analysisEnv

            case eitherAnalysisResult of
                Right analysisResult -> do
                    let errorCodes = extractErrorCodes analysisResult
                    let commentRes = prettyFormatAnalysis analysisResult ticketInfo

                    let fErrorCode = foldr (\errorCode acc -> errorCode <> ";" <> acc) "" errorCodes

                    printText fErrorCode

                    pure ZendeskResponse
                        { zrTicketId    = tiId
                        , zrComment     = commentRes
                        , zrTags        = errorCodes
                        , zrIsPublic    = cfgIsCommentPublic
                        }

                Left _ -> do

                    printText . renderTicketStatus $ NoKnownIssue

                    pure ZendeskResponse
                        { zrTicketId    = tiId
                        , zrComment     = prettyFormatNoIssues ticketInfo
                        , zrTags        = [renderTicketStatus NoKnownIssue]
                        , zrIsPublic    = cfgIsCommentPublic
                        }

-- | Filter analyzed tickets
filterAnalyzedTickets :: [TicketInfo] -> [TicketInfo]
filterAnalyzedTickets ticketsInfo =
    filter ticketsFilter ticketsInfo
  where
    ticketsFilter :: TicketInfo -> Bool
    ticketsFilter ticketInfo =
        isTicketAnalyzed ticketInfo && isTicketOpen ticketInfo && isTicketBlacklisted ticketInfo

    isTicketAnalyzed :: TicketInfo -> Bool
    isTicketAnalyzed TicketInfo{..} = (renderTicketStatus AnalyzedByScriptV1_0) `notElem` (getTicketTags tiTags)
    -- ^ This is showing that something is wrong...

    isTicketOpen :: TicketInfo -> Bool
    isTicketOpen TicketInfo{..} = tiStatus == TicketStatus "open" -- || ticketStatus == "new"

    -- | If we have a ticket we are having issues with...
    isTicketBlacklisted :: TicketInfo -> Bool
    isTicketBlacklisted TicketInfo{..} = tiId `notElem` [TicketId 9377,TicketId 10815]


