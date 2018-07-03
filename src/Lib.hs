{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
    ( runZendeskMain
    , collectEmails
    , getZendeskResponses
    , processTicket
    , processTickets
    , fetchTickets
    , showStatistics
    , listAndSortTickets
    , filterAnalyzedTickets
    ) where

import           Universum

import           Data.Attoparsec.Text.Lazy (eitherResult, parse)
import           Data.Text (isInfixOf, stripEnd)
import           System.Directory (createDirectoryIfMissing)

import           CLI (CLI (..), getCliArgs)
import           DataSource (App, Attachment (..), AttachmentContent (..), Comment (..),
                             CommentBody (..), Config (..), IOLayer (..), TicketId (..),
                             TicketInfo (..), TicketStatus (..), TicketTag (..), TicketTags (..),
                             User (..), UserId (..), ZendeskLayer (..), ZendeskResponse (..),
                             asksIOLayer, asksZendeskLayer, assignToPath, defaultConfig,
                             knowledgebasePath, renderTicketStatus, runApp, tokenPath)
import           LogAnalysis.Classifier (extractErrorCodes, extractIssuesFromLogs,
                                         prettyFormatAnalysis, prettyFormatLogReadError,
                                         prettyFormatNoIssues, prettyFormatNoLogs)
import           LogAnalysis.KnowledgeCSVParser (parseKnowLedgeBase)
import           LogAnalysis.Types (ErrorCode (..), Knowledge, renderErrorCode, setupAnalysis)
import           Statistics (showStatistics)
import           Util (extractLogsFromZip)

------------------------------------------------------------
-- Functions
------------------------------------------------------------

runZendeskMain :: IO ()
runZendeskMain = do
    createDirectoryIfMissing True "logs"
    args <- getCliArgs
    putTextLn "Welcome to Zendesk classifier!"
    token <- readFile tokenPath  -- Zendesk token
    assignFile <- readFile assignToPath  -- Select assignee
    knowledges <- setupKnowledgebaseEnv knowledgebasePath
    assignTo <- case readEither assignFile of
        Right agentid -> return agentid
        Left  err     -> error err

    let cfg = defaultConfig
                   { cfgToken         = stripEnd token
                   , cfgAssignTo      = assignTo
                   , cfgAgentId       = assignTo
                   , cfgKnowledgebase = knowledges
                   }

    -- At this point, the configuration is set up and there is no point in using a pure IO.
    case args of
        CollectEmails            -> runApp collectEmails cfg
        FetchAgents              -> void $ runApp fetchAgents cfg
        FetchTickets             -> runApp fetchAndShowTickets cfg
        (ProcessTicket ticketId) -> void $ runApp (processTicket (TicketId ticketId)) cfg
        ProcessTickets           -> void $ runApp processTickets cfg
        ShowStatistics           -> void $ runApp (fetchTickets >>= showStatistics) cfg

-- TODO(hs): Remove this function since it's not used
collectEmails :: App ()
collectEmails = do
    cfg <- ask

    let email   = cfgEmail cfg
    let userId  = UserId . fromIntegral $ cfgAgentId cfg

    -- We first fetch the function from the configuration
    listTickets <- asksZendeskLayer zlListAssignedTickets
    putTextLn $ "Classifier is going to extract emails requested by: " <> email
    tickets     <- listTickets userId
    putTextLn $ "There are " <> show (length tickets) <> " tickets requested by this user."
    let ticketIds = foldr (\TicketInfo{..} acc -> tiId : acc) [] tickets
    mapM_ extractEmailAddress ticketIds

fetchAgents :: App [User]
fetchAgents = do
    Config{..}      <- ask
    listAdminAgents <- asksZendeskLayer zlListAdminAgents
    printText       <- asksIOLayer iolPrintText

    printText "Fetching Zendesk agents"

    agents          <- listAdminAgents

    mapM_ print agents
    pure agents

processTicket :: TicketId -> App (Maybe ZendeskResponse)
processTicket tId = do
    printText <- asksIOLayer iolPrintText
    -- Printing id before inspecting the ticket so that when the process stops by the
    -- corrupted log file, we know which id to blacklist.
    printText $ "Analyzing ticket id: " <> show (getTicketId tId)

    -- We first fetch the function from the configuration
    getTicketInfo       <- asksZendeskLayer zlGetTicketInfo
    mTicketInfo         <- getTicketInfo tId

    getTicketComments   <- asksZendeskLayer zlGetTicketComments
    comments            <- getTicketComments tId
    let attachments     = getAttachmentsFromComment comments
    let ticketInfo      = fromMaybe (error "No ticket info") mTicketInfo
    zendeskResponse     <- getZendeskResponses comments attachments ticketInfo
    postTicketComment   <- asksZendeskLayer zlPostTicketComment

    whenJust zendeskResponse $ \response -> do
        postTicketComment ticketInfo response
        let tags = getTicketTags $ zrTags response
        forM_ tags $ \tag -> do
            let formattedTicketIdAndTag = show (getTicketId tId) <> " " <> tag
            printText formattedTicketIdAndTag
            appendF <- asksIOLayer iolAppendFile
            appendF "logs/analysis-result.log" (formattedTicketIdAndTag <> "\n")

    pure zendeskResponse

processTickets :: App ()
processTickets = do
    sortedTicketIds             <- listAndSortTickets
    sortedUnassignedTicketIds   <- listAndSortUnassignedTickets

    let allTickets = sortedTicketIds <> sortedUnassignedTicketIds

    _                   <- mapM (processTicket . tiId) allTickets

    putTextLn "All the tickets has been processed."

fetchTickets :: App [TicketInfo]
fetchTickets = do
    sortedTicketIds             <- listAndSortTickets
    sortedUnassignedTicketIds   <- listAndSortUnassignedTickets

    let allTickets = sortedTicketIds <> sortedUnassignedTicketIds
    return allTickets

fetchAndShowTickets :: App ()
fetchAndShowTickets = do
    tickets <- fetchTickets
    mapM_ (putTextLn . show) tickets
    putTextLn "All the tickets has been processed."

-- TODO(ks): Extract repeating code, generalize.
listAndSortTickets :: App [TicketInfo]
listAndSortTickets = do

    Config{..}  <- ask

    listAgents <- asksZendeskLayer zlListAdminAgents
    agents <- listAgents

    let agentIds :: [UserId]
        agentIds = map uId agents
    -- We first fetch the function from the configuration
    listTickets <- asksZendeskLayer zlListAssignedTickets
    printText   <- asksIOLayer iolPrintText

    printText "Classifier is going to process tickets assigned to agents"

    ticketInfos     <- map concat $ traverse listTickets agentIds

    let filteredTicketIds = filterAnalyzedTickets ticketInfos
    let sortedTicketIds   = sortBy compare filteredTicketIds

    printText $ "There are " <> show (length sortedTicketIds) <> " unanalyzed tickets."
    printText "Processing tickets, this may take hours to finish."

    pure sortedTicketIds

listAndSortUnassignedTickets :: App [TicketInfo]
listAndSortUnassignedTickets = do

    -- We first fetch the function from the configuration
    listUnassignedTickets   <- asksZendeskLayer zlListUnassignedTickets
    printText               <- asksIOLayer iolPrintText

    printText "Classifier is going to process tickets assigned to agents"

    ticketInfos             <- listUnassignedTickets

    let filteredTicketIds   = filterAnalyzedTickets ticketInfos
    let sortedTicketIds     = sortBy compare filteredTicketIds

    printText $ "There are " <> show (length sortedTicketIds) <> " unanalyzed and unassigned tickets."
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

-- | Get zendesk responses
-- | Returns with maybe because it could return no response
getZendeskResponses :: [Comment] -> [Attachment] -> TicketInfo -> App (Maybe ZendeskResponse)
getZendeskResponses comments attachments ticketInfo
    | not (null attachments) = inspectAttachments ticketInfo attachments
    | not (null comments)    = Just <$> responseNoLogs ticketInfo
    | otherwise              = return Nothing

-- | Inspect only the latest attachment. We could propagate this
-- @Maybe@ upwards or use an @Either@ which will go hand in hand
-- with the idea that we need to improve our exception handling.
inspectAttachments :: TicketInfo -> [Attachment] -> App (Maybe ZendeskResponse)
inspectAttachments ticketInfo attachments = runMaybeT $ do

    config          <- ask
    getAttachment   <- asksZendeskLayer zlGetAttachment

    let lastAttach :: Maybe Attachment
        lastAttach = safeHead . reverse . sort $ attachments

    lastAttachment  <- MaybeT . pure $ lastAttach
    att             <- MaybeT $ getAttachment lastAttachment

    pure $ inspectAttachment config ticketInfo att


-- | Given number of file of inspect, knowledgebase and attachment,
-- analyze the logs and return the results.
inspectAttachment :: Config -> TicketInfo -> AttachmentContent -> ZendeskResponse
inspectAttachment Config{..} ticketInfo@TicketInfo{..} attContent = do

    let rawLog      = getAttachmentContent attContent
    let results     = extractLogsFromZip cfgNumOfLogsToAnalyze rawLog

    case results of
        Left _ -> do

            ZendeskResponse
                { zrTicketId    = tiId
                , zrComment     = prettyFormatLogReadError ticketInfo
                , zrTags        = TicketTags [renderErrorCode SentLogCorrupted]
                , zrIsPublic    = cfgIsCommentPublic
                }
        Right result -> do
            let analysisEnv             = setupAnalysis cfgKnowledgebase
            let eitherAnalysisResult    = extractIssuesFromLogs result analysisEnv

            case eitherAnalysisResult of
                Right analysisResult -> do
                    let errorCodes = extractErrorCodes analysisResult
                    let commentRes = prettyFormatAnalysis analysisResult ticketInfo

                    ZendeskResponse
                        { zrTicketId    = tiId
                        , zrComment     = commentRes
                        , zrTags        = TicketTags errorCodes
                        , zrIsPublic    = cfgIsCommentPublic
                        }

                Left _ -> do

                    ZendeskResponse
                        { zrTicketId    = tiId
                        , zrComment     = prettyFormatNoIssues ticketInfo
                        , zrTags        = TicketTags [renderTicketStatus NoKnownIssue]
                        , zrIsPublic    = cfgIsCommentPublic
                        }

responseNoLogs :: TicketInfo -> App ZendeskResponse
responseNoLogs TicketInfo{..} = do
    Config {..} <- ask
    pure ZendeskResponse
             { zrTicketId = tiId
             , zrComment  = prettyFormatNoLogs
             , zrTags     = TicketTags [renderTicketStatus NoLogAttached]
             , zrIsPublic = cfgIsCommentPublic
             }

-- | Filter analyzed tickets
filterAnalyzedTickets :: [TicketInfo] -> [TicketInfo]
filterAnalyzedTickets ticketsInfo =
    filter ticketsFilter ticketsInfo
  where
    ticketsFilter :: TicketInfo -> Bool
    ticketsFilter ticketInfo =
           isTicketAnalyzed ticketInfo
        && isTicketOpen ticketInfo
        && isTicketBlacklisted ticketInfo
        && isTicketInGoguenTestnet ticketInfo

    analyzedTags :: [Text]
    analyzedTags = map renderTicketStatus [AnalyzedByScriptV1_0, AnalyzedByScriptV1_1]

    isTicketAnalyzed :: TicketInfo -> Bool
    isTicketAnalyzed TicketInfo{..} = all (\analyzedTag -> analyzedTag `notElem` (getTicketTags tiTags)) analyzedTags
    -- ^ This is showing that something is wrong...

    unsolvedTicketStatus :: [TicketStatus]
    unsolvedTicketStatus = map TicketStatus ["new", "open", "hold", "pending"]

    isTicketOpen :: TicketInfo -> Bool
    isTicketOpen TicketInfo{..} = tiStatus `elem` unsolvedTicketStatus-- || ticketStatus == "new"

    -- | If we have a ticket we are having issues with...
    isTicketBlacklisted :: TicketInfo -> Bool
    isTicketBlacklisted TicketInfo{..} = tiId `notElem` [TicketId 9377,TicketId 10815, TicketId 15066]

    isTicketInGoguenTestnet :: TicketInfo -> Bool
    isTicketInGoguenTestnet TicketInfo{..} = "goguen_testnets" `notElem` getTicketTags tiTags
