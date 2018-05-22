{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Lib
       ( runZendeskMain
       ) where

import           Universum

import           Data.Text (stripEnd, isInfixOf)
import           Data.Attoparsec.Text.Lazy (eitherResult, parse)

import           LogAnalysis.Types
import           LogAnalysis.Classifier
import           LogAnalysis.KnowledgeCSVParser
import           Util
import           Config
import           CLI
import           Zendesk


------------------------------------------------------------
-- Configuration
------------------------------------------------------------

newtype App a = App (ReaderT Config IO a)
    deriving ( Applicative
             , Functor
             , Monad
             , MonadReader Config
             , MonadIO
             )

runApp :: App a -> Config -> IO a
runApp (App a) = runReaderT a

------------------------------------------------------------
-- Functions
------------------------------------------------------------

-- TODO(ks): This is temporary!

getTicketInfo
    :: (MonadIO m, MonadReader Config m)
    => TicketId
    -> m TicketInfo
getTicketInfo = zlGetTicketInfo basicZendeskLayer

listTickets
    :: (MonadIO m, MonadReader Config m)
    => RequestType
    -> m [TicketInfo]
listTickets = zlListTickets basicZendeskLayer

postTicketComment
    :: (MonadIO m, MonadReader Config m)
    => TicketId
    -> Text
    -> [Text]
    -> Bool
    -> m ()
postTicketComment = zlPostTicketComment basicZendeskLayer

getAttachment
    :: (MonadIO m, MonadReader Config m)
    => Attachment
    -> m LByteString
getAttachment = zlGetAttachment basicZendeskLayer

getTicketComments
    :: (MonadIO m, MonadReader Config m)
    => TicketId
    -> m [Comment]
getTicketComments = zlGetTicketComments basicZendeskLayer


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
    agentId <- runApp (zlGetAgentId basicZendeskLayer) cfg'
    let cfg = cfg' { cfgAgentId = agentId }
    case args of
        -- Process all the tikects that are requested by agent
        CollectEmails -> do
            putTextLn $  "Classifier is going to extract emails requested by: " <> cfgEmail cfg
            tickets <- runApp (listTickets Requested) cfg
            putTextLn $ "There are " <> show (length tickets) <> " tickets requested by this user."
            let ticketIds = foldr (\TicketInfo{..} acc -> ticketId : acc) [] tickets
            mapM_ (\tid -> runApp (extractEmailAddress tid) cfg) ticketIds
        -- Process given ticket
        (ProcessTicket ticketId) -> do
            putTextLn "Processing single ticket"
            ticketInfo <- runApp (getTicketInfo ticketId) cfg
            runApp (processTicketAndId ticketInfo) cfg
            putTextLn "Process finished, please see the following url"
            putTextLn $ "https://iohk.zendesk.com/agent/tickets/" <> show ticketId
        -- Process all the tickets (WARNING: This is really long process)
        ProcessTickets -> do
            sortedTicketIds <- processBatchTickets cfg
            mapM_ (\ticketInfo -> runApp (processTicketAndId ticketInfo) cfg) sortedTicketIds
            putTextLn "All the tickets has been processed."
        -- Fetch all the tickets
        FetchTickets -> do
            sortedTicketIds <- processBatchTickets cfg
            mapM_ (putTextLn . show) sortedTicketIds
            putTextLn "All the tickets has been processed."
        -- Collect statistics
        ShowStatistics -> do
            putTextLn $ "Classifier is going to gather ticket information assigned to: "
                <> cfgEmail cfg
            printWarning
            tickets <- runApp (listTickets Assigned) cfg
            printTicketCountMessage tickets (cfgEmail cfg)

processBatchTickets :: Config -> IO [TicketInfo]
processBatchTickets cfg = do
    putTextLn $  "Classifier is going to process tickets assign to: " <> cfgEmail cfg
    printWarning
    tickets <- runApp (listTickets Assigned) cfg

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
    comments <- getTicketComments ticketId
    let commentWithEmail = cBody $ fromMaybe (error "No comment") (safeHead comments)
    let emailAddress = fromMaybe (error "No email") (safeHead $ lines commentWithEmail)
    liftIO $ guard ("@" `isInfixOf` emailAddress)
    liftIO $ appendFile "emailAddress.txt" (emailAddress <> "\n")
    liftIO $ putTextLn emailAddress

-- | Process specifig ticket id (can be used for testing) only inspects the one's with logs
processTicketAndId :: TicketInfo -> App ()
processTicketAndId ticketInfo@TicketInfo{..} = do
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
inspectAttachmentAndPostComment ticketInfo@TicketInfo{..} attachment = do
    liftIO $ putTextLn $ "Analyzing ticket: " <> show ticketInfo
    zendeskResponse <- inspectAttachment ticketInfo attachment

    let comment         = zrComment zendeskResponse
    let tags            = zrTags zendeskResponse
    let isPublicComment = zrIsPublic zendeskResponse

    postTicketComment ticketId comment tags isPublicComment

-- | Given number of file of inspect, knowledgebase and attachment,
-- analyze the logs and return the results.
--
-- The results are following:
--
-- __(comment, tags, bool of whether is should be public comment)__
inspectAttachment :: TicketInfo -> Attachment -> App ZendeskResponse
inspectAttachment ticketInfo att = do
    config@Config{..} <- ask

    rawlog <- liftIO $ runApp (getAttachment att) config -- Get attachment
    let results = extractLogsFromZip cfgNumOfLogsToAnalyze rawlog

    case results of
        Left _ -> do

            liftIO . putStrLn . renderErrorCode $ SentLogCorrupted

            pure ZendeskResponse
                { zrComment     = prettyFormatLogReadError ticketInfo
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
                        { zrComment     = commentRes
                        , zrTags        = errorCodes
                        , zrIsPublic    = cfgIsCommentPublic
                        }

                Left _ -> do

                    liftIO . putStrLn . renderTicketStatus $ NoKnownIssue

                    pure ZendeskResponse
                        { zrComment     = prettyFormatNoIssues ticketInfo
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




