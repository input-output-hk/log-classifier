{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Lib
    ( runZendeskMain
    , collectEmails
    , processTicket
    , processTickets
    , fetchTickets
    , showStatistics
    ) where

import           Universum

import           Data.Attoparsec.Text.Lazy (eitherResult, parse)
import           Data.Text (isInfixOf, stripEnd)

import           CLI (CLI (..), getCliArgs)
import           LogAnalysis.Classifier (extractErrorCodes, extractIssuesFromLogs,
                                         prettyFormatAnalysis, prettyFormatLogReadError,
                                         prettyFormatNoIssues)
import           LogAnalysis.KnowledgeCSVParser (parseKnowLedgeBase)
import           LogAnalysis.Types (ErrorCode (..), Knowledge, renderErrorCode, setupAnalysis)
import           Util (extractLogsFromZip)
import           Zendesk (App, Attachment (..), Comment (..), Config (..), RequestType (..),
                          TicketId, TicketInfo (..), TicketTag (..), ZendeskLayer (..),
                          ZendeskResponse (..), asksZendeskLayer, assignToPath, defaultConfig,
                          knowledgebasePath, renderTicketStatus, runApp, tokenPath)

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
    let cfg' = defaultConfig
                   { cfgToken = stripEnd token
                   , cfgAssignTo = assignTo
                   , cfgKnowledgebase = knowledges
                   }
    let getAgentId = zlGetAgentId . cfgZendeskLayer $ cfg'
    agentId <- runApp getAgentId cfg'
    let cfg = cfg' { cfgAgentId = agentId }
    -- At this point, the configuration is set up and there is no point in using a pure IO.
    case args of
        CollectEmails            -> runApp collectEmails cfg
        (ProcessTicket ticketId) -> runApp (processTicket ticketId) cfg
        ProcessTickets           -> runApp processTickets cfg
        FetchTickets             -> runApp fetchTickets cfg
        ShowStatistics           -> runApp showStatistics cfg


collectEmails :: App ()
collectEmails = do
    cfg <- ask
    -- We first fetch the function from the configuration
    listTickets <- asksZendeskLayer zlListTickets
    putTextLn $  "Classifier is going to extract emails requested by: " <> cfgEmail cfg
    tickets <- listTickets Requested
    putTextLn $ "There are " <> show (length tickets) <> " tickets requested by this user."
    let ticketIds = foldr (\TicketInfo{..} acc -> ticketId : acc) [] tickets
    mapM_ extractEmailAddress ticketIds

processTicket :: TicketId -> App ()
processTicket ticketId = do
    -- We first fetch the function from the configuration
    getTicketInfo <- asksZendeskLayer zlGetTicketInfo
    putTextLn "Processing single ticket"
    ticketInfo <- getTicketInfo ticketId
    processTicketAndId ticketInfo
    putTextLn "Process finished, please see the following url"
    putTextLn $ "https://iohk.zendesk.com/agent/tickets/" <> show ticketId

processTickets :: App ()
processTickets = do
    cfg <- ask
    sortedTicketIds <- processBatchTickets cfg
    mapM_ processTicketAndId sortedTicketIds
    putTextLn "All the tickets has been processed."

fetchTickets :: App ()
fetchTickets = do
    cfg <- ask
    sortedTicketIds <- processBatchTickets cfg
    mapM_ (putTextLn . show) sortedTicketIds
    putTextLn "All the tickets has been processed."

showStatistics :: App ()
showStatistics = do
    cfg <- ask
    -- We first fetch the function from the configuration
    listTickets <- asksZendeskLayer zlListTickets

    putTextLn $ "Classifier is going to gather ticket information assigned to: " <> cfgEmail cfg
    liftIO printWarning
    tickets <- listTickets Assigned
    liftIO $ printTicketCountMessage tickets (cfgEmail cfg)


processBatchTickets :: Config -> App [TicketInfo]
processBatchTickets cfg = do

    -- We first fetch the function from the configuration
    listTickets <- asksZendeskLayer zlListTickets

    putTextLn $  "Classifier is going to process tickets assign to: " <> cfgEmail cfg
    liftIO printWarning
    tickets <- listTickets Assigned

    let filteredTicketIds = filterAnalyzedTickets tickets
    let sortedTicketIds   = sortBy compare filteredTicketIds

    putTextLn $ "There are " <> show (length sortedTicketIds) <> " unanalyzed tickets."
    putTextLn "Processing tickets, this may take hours to finish."

    pure sortedTicketIds


-- | Warning
printWarning :: IO ()
printWarning = putTextLn "Note that this process may take a while. Please do not kill the process"

-- | Print how many tickets are assinged, analyzed, and unanalyzed
printTicketCountMessage :: [TicketInfo] -> Text -> IO ()
printTicketCountMessage tickets email = do
    let ticketCount = length tickets
    putTextLn "Done!"
    putTextLn $ "There are currently " <> show ticketCount
        <> " tickets in the system assigned to " <> email
    let filteredTicketCount = length $ filterAnalyzedTickets tickets
    putTextLn $ show (ticketCount - filteredTicketCount)
        <> " tickets has been analyzed by the classifier."
    putTextLn $ show filteredTicketCount <> " tickets are not analyzed."
    putTextLn "Below are statistics:"
    let tagGroups = sortTickets tickets
    mapM_ (\(tag, count) -> putTextLn $ tag <> ": " <> show count) tagGroups

-- | Sort the ticket so we can see the statistics
sortTickets :: [TicketInfo] -> [(Text, Int)]
sortTickets tickets =
    let extractedTags = foldr (\TicketInfo{..} acc -> ticketTags <> acc) [] tickets  -- Extract tags from tickets
        tags2Filter   = ["s3", "s2", "cannot-sync", "closed-by-merge"
                        , "web_widget", "analyzed-by-script"]
        filteredTags  = filter (`notElem` tags2Filter) extractedTags  -- Filter tags
        groupByTags :: [ Text ] -> [(Text, Int)]
        groupByTags ts = map (\l@(x:_) -> (x, length l)) (group $ sort ts)  -- Group them
    in  groupByTags filteredTags

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
    let commentWithEmail = cBody $ fromMaybe (error "No comment") (safeHead comments)
    let emailAddress = fromMaybe (error "No email") (safeHead $ lines commentWithEmail)
    liftIO $ guard ("@" `isInfixOf` emailAddress)
    liftIO $ appendFile "emailAddress.txt" (emailAddress <> "\n")
    liftIO $ putTextLn emailAddress

-- | Process specifig ticket id (can be used for testing) only inspects the one's with logs
processTicketAndId :: TicketInfo -> App ()
processTicketAndId ticketInfo@TicketInfo{..} = do

    getTicketComments <- asksZendeskLayer zlGetTicketComments

    comments <- getTicketComments ticketId

    -- Filter tickets without logs
    -- Could analyze the comments but I don't see it useful..
    let commentsWithAttachments :: [Comment]
        commentsWithAttachments = filter (\x -> length (cAttachments x) > 0) comments

    -- Filter out ticket without logs
    let attachments :: [Attachment]
        attachments = concatMap cAttachments commentsWithAttachments

    let justLogs :: [Attachment]
        justLogs = filter (\x -> "application/zip" == aContentType x) attachments

    mapM_ (inspectAttachmentAndPostComment ticketInfo) justLogs

-- | Inspect attachment then post comment to the ticket
inspectAttachmentAndPostComment :: TicketInfo -> Attachment -> App ()
inspectAttachmentAndPostComment ticketInfo attachment = do
    liftIO $ putTextLn $ "Analyzing ticket: " <> show ticketInfo
    zendeskResponse <- inspectAttachment ticketInfo attachment

    postTicketComment <- asksZendeskLayer zlPostTicketComment
    postTicketComment zendeskResponse

-- | Given number of file of inspect, knowledgebase and attachment,
-- analyze the logs and return the results.
--
-- The results are following:
--
-- __(comment, tags, bool of whether is should be public comment)__
inspectAttachment :: TicketInfo -> Attachment -> App ZendeskResponse
inspectAttachment ticketInfo@TicketInfo{..} att = do

    Config{..} <- ask

    getAttachment <- asksZendeskLayer zlGetAttachment

    rawlog <- getAttachment att -- Get attachment
    let results = extractLogsFromZip cfgNumOfLogsToAnalyze rawlog

    case results of
        Left _ -> do

            liftIO . putStrLn . renderErrorCode $ SentLogCorrupted

            pure ZendeskResponse
                { zrTicketId    = ticketId
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

                    liftIO . putTextLn $ fErrorCode

                    pure ZendeskResponse
                        { zrTicketId    = ticketId
                        , zrComment     = commentRes
                        , zrTags        = errorCodes
                        , zrIsPublic    = cfgIsCommentPublic
                        }

                Left _ -> do

                    liftIO . putStrLn . renderTicketStatus $ NoKnownIssue

                    pure ZendeskResponse
                        { zrTicketId    = ticketId
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
    isTicketAnalyzed TicketInfo{..} = (renderTicketStatus AnalyzedByScriptV1_0) `notElem` ticketTags

    isTicketOpen :: TicketInfo -> Bool
    isTicketOpen TicketInfo{..} = ticketStatus == "open" -- || ticketStatus == "new"

    -- | If we have a ticket we are having issues with...
    isTicketBlacklisted :: TicketInfo -> Bool
    isTicketBlacklisted TicketInfo{..} = ticketId `notElem` [9377,10815]

