{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
    ( runZendeskMain
    , getZendeskResponses
    , getAttachmentsFromComment
    , processTicket
    , processTicketSafe
    , processTickets
    , fetchTickets
    , showStatistics
    , listAndSortTickets
    , filterAnalyzedTickets
    , exportZendeskDataToLocalDB
    -- * Optional
    , fetchTicket
    , fetchTicketComments
    ) where

import           Universum

import           UnliftIO.Async (mapConcurrently)

import           Data.Attoparsec.Text.Lazy (eitherResult, parse)
import qualified Data.ByteString.Lazy as BS
import           Data.List (nub)
import           Data.Text (stripEnd)

import           System.Directory (createDirectoryIfMissing)
import           System.IO (BufferMode (..), hSetBuffering)

import           CLI (CLI (..), getCliArgs)
import           Configuration (defaultConfig)
import           DataSource (App, Attachment (..), AttachmentContent (..), Comment (..),
                             Config (..), DBLayer (..), DataLayer (..),
                             DeletedTicket (..), ExportFromTime (..), IOLayer (..), TicketId (..),
                             TicketInfo (..), TicketStatus (..), TicketTag (..), TicketTags (..),
                             User (..), UserId (..), ZendeskResponse (..), asksDBLayer, asksIOLayer,
                             assignToPath, basicDataLayer, connPoolDBLayer,
                             createProdConnectionPool, knowledgebasePath, renderTicketStatus,
                             runApp, tokenPath)

import           Exceptions (ClassifierExceptions (..), ProcessTicketExceptions (..),
                             ZipFileExceptions (..))
import           Http.Layer (basicHTTPNetworkLayer)
import           Http.Queue (createSchedulerConfig, runScheduler)

import           LogAnalysis.Classifier (extractErrorCodes, extractIssuesFromLogs,
                                         prettyFormatAnalysis, prettyFormatLogReadError,
                                         prettyFormatNoIssues, prettyFormatNoLogs)
import           LogAnalysis.Exceptions (LogAnalysisException (..))
import           LogAnalysis.KnowledgeCSVParser (parseKnowLedgeBase)
import           LogAnalysis.Types (ErrorCode (..), Knowledge, renderErrorCode, setupAnalysis)
import           Statistics (showStatistics)
import           Util (extractLogsFromZip)

------------------------------------------------------------
-- Functions
------------------------------------------------------------

-- | Main function for the log-classifier
runZendeskMain :: IO ()
runZendeskMain = do
    createDirectoryIfMissing True "logs"
    hSetBuffering stdout NoBuffering

    args        <- getCliArgs

    putTextLn "Welcome to Zendesk classifier!"
    token       <- readFile tokenPath       -- Zendesk token
    assignFile  <- readFile assignToPath    -- Select assignee
    knowledges  <- setupKnowledgebaseEnv knowledgebasePath
    assignTo    <- case readEither assignFile of
        Right agentid -> return agentid
        Left  _       -> throwM FailedToReadAssignFile

    connPool    <- createProdConnectionPool

    let cfg     = defaultConfig
                    { cfgToken         = stripEnd token
                    , cfgAssignTo      = assignTo
                    , cfgAgentId       = UserId (fromIntegral assignTo)
                    , cfgKnowledgebase = knowledges
                    , cfgDBLayer       = connPoolDBLayer connPool
                    }

    -- We really need to get rid of this...
    -- We don't need any configuration for this, we could move it down to functions.
    dataLayer   <- runApp createBasicDataLayer cfg

    -- At this point, the configuration is set up and there is no point in using a pure IO.
    case args of
        FetchAgents                     -> void $ runApp (fetchAgents dataLayer) cfg
        FetchTickets                    -> runApp (fetchAndShowTickets dataLayer) cfg
        FetchTicketsFromTime fromTime   -> runApp (fetchAndShowTicketsFrom dataLayer fromTime) cfg
        (ProcessTicket ticketId)        -> void $ runApp (processTicketSafe dataLayer (TicketId ticketId)) cfg
        ProcessTickets                  -> void $ runApp (processTickets dataLayer) cfg
        ProcessTicketsFromTime fromTime -> runApp (processTicketsFromTime dataLayer fromTime) cfg
        ShowStatistics                  ->
            void $ runApp (fetchTickets dataLayer >>= (showStatistics dataLayer)) cfg
        InspectLocalZip filePath        -> runApp (inspectLocalZipAttachment filePath) cfg
        ExportData fromTime             -> void $ runApp (exportZendeskDataToLocalDB dataLayer fromTime) cfg
  where
    -- Read CSV file and setup knowledge base
    setupKnowledgebaseEnv :: FilePath -> IO [Knowledge]
    setupKnowledgebaseEnv path = do
        kfile <- toLText <$> readFile path
        let kb = parse parseKnowLedgeBase kfile
        case eitherResult kb of
            Left _   -> throwM FailedToParseKnowledgebase
            Right ks -> return ks

    -- | Create a basic @DataLayer@.
    createBasicDataLayer :: App (DataLayer App)
    createBasicDataLayer = do
        -- for now, we limit the rate-limit to 700/minute, which is lower then
        -- 750/minute, but takes into consideration that other things might be called
        -- at the same time as well.
        shedConfig  <- createSchedulerConfig 700

        -- Run the sheduler which resets the number of requests to 0 every minute.
        _           <- runScheduler shedConfig

        -- create the data layer
        pure $ basicDataLayer (basicHTTPNetworkLayer shedConfig)


-- | The function for exporting Zendesk data to our local DB so
-- we can have faster analysis and runs.
-- We expect that the local database exists and has the correct schema.
exportZendeskDataToLocalDB
    :: DataLayer App
    -> ExportFromTime
    -> App [TicketInfo]
exportZendeskDataToLocalDB dataLayer exportFromTime = do

    deleteAllData               <- asksDBLayer dlDeleteAllData

    notDeletedExportedTickets   <- fetchTicketsExportedFromTime dataLayer exportFromTime

    -- Map concurrently if required.
    allTicketData               <-  mapConcurrently fetchTicketData notDeletedExportedTickets

    -- Clear the data.
    deleteAllData

    -- Save all the data.
    _           <- mapM saveTicketDataToLocalDB allTicketData

    -- TODO(ks): We currently don't download the attachment since that
    -- would take a ton of time and would require probably up to 100Gb of space!
    pure notDeletedExportedTickets
  where
    -- | Get ticket comments and create a sum type out of them.
    fetchTicketData :: TicketInfo -> App (TicketInfo,[Comment])
    fetchTicketData ticket = do

        let getTicketComments       = zlGetTicketComments dataLayer

        -- First fetch comments, FAIL-FAST
        ticketComments              <- getTicketComments $ tiId ticket

        pure (ticket, ticketComments)

-- | Fetch all tickets changed from a specific date.
fetchTicketsExportedFromTime :: DataLayer App -> ExportFromTime -> App [TicketInfo]
fetchTicketsExportedFromTime dataLayer exportFromTime = do

    let listDeletedTickets      = zlListDeletedTickets dataLayer
    let exportTickets           = zlExportTickets dataLayer

    -- Yeah, not sure why this happens. Very weird.
    exportedTickets             <- nub <$> exportTickets exportFromTime

    let filteredTicketIds = filterAnalyzedTickets exportedTickets
    let sortedTicketIds   = sortBy compare filteredTicketIds

    -- Yeah, there is a strange behaviour when we export tickets,
    -- we might get deleted tickets as well.
    deletedTickets              <- listDeletedTickets

    pure . sort $ filter (not . isDeletedTicket deletedTickets) sortedTicketIds
  where
    -- | Filter for deleted tickets.
    isDeletedTicket :: [DeletedTicket] -> TicketInfo -> Bool
    isDeletedTicket dTickets TicketInfo{..} =
        tiId `elem` map dtId dTickets

-- | We are missing an intermediate step here to test and separate.
saveTicketDataToLocalDB :: (TicketInfo,[Comment]) -> App ()
saveTicketDataToLocalDB (ticket, ticketComments) = do

    insertTicketInfo            <- asksDBLayer dlInsertTicketInfo
    insertTicketComments        <- asksDBLayer dlInsertTicketComments
    insertCommentAttachments    <- asksDBLayer dlInsertCommentAttachments

    let ticketId = tiId ticket

    insertTicketInfo ticket

    -- For all the ticket comments
    void $ forM ticketComments $ \ticketComment -> do
        insertTicketComments ticketId ticketComment

        -- For all the ticket comment attachments
        forM (cAttachments ticketComment) $ \ticketAttachment ->
            insertCommentAttachments ticketComment ticketAttachment

    pure ()

fetchAgents :: DataLayer App -> App [User]
fetchAgents dataLayer = do
    Config{..}          <- ask

    let listAdminAgents = zlListAdminAgents dataLayer
    printText           <- asksIOLayer iolPrintText

    printText "Fetching Zendesk agents"

    agents              <- listAdminAgents

    mapM_ print agents
    pure agents

-- | 'processTicket' with exception handling
processTicketSafe :: DataLayer App ->TicketId -> App ()
processTicketSafe dataLayer tId = catch (void $ processTicket dataLayer tId)
    -- Print and log any exceptions related to process ticket
    -- TODO(ks): Remove IO from here, return the error.
    (\(e :: ProcessTicketExceptions) -> do
        printText <- asksIOLayer iolPrintText
        printText $ "Error on TicketId " <> show (getTicketId tId) <> ":" <> show e
        -- TODO(hs): Implement concurrent logging
        appendF <- asksIOLayer iolAppendFile
        appendF "./logs/errors.log" (show e <> "\n"))

-- | Process ticket with given 'TicketId'
processTicket :: HasCallStack => DataLayer App -> TicketId -> App ZendeskResponse
processTicket dataLayer tId = do

    -- We see 3 HTTP calls here.
    let getTicketInfo       = zlGetTicketInfo dataLayer
    let getTicketComments   = zlGetTicketComments dataLayer
    let postTicketComment   = zlPostTicketComment dataLayer

    mTicketInfo         <- getTicketInfo tId
    comments            <- getTicketComments tId

    let attachments     = getAttachmentsFromComment comments
    case mTicketInfo of
        Nothing         -> throwM $ TicketInfoNotFound tId
        Just ticketInfo -> do
            zendeskResponse <- getZendeskResponses dataLayer comments attachments ticketInfo

            -- post ticket comment
            postTicketComment ticketInfo zendeskResponse

            pure zendeskResponse

-- | When we want to process all tickets from a specific time onwards.
-- Run in parallel.
processTicketsFromTime :: DataLayer App -> ExportFromTime -> App ()
processTicketsFromTime dataLayer exportFromTime = do

    allTickets          <- fetchTicketsExportedFromTime dataLayer exportFromTime

    putTextLn $ "There are " <> show (length allTickets) <> " tickets."

    mZendeskResponses   <- mapConcurrently (processTicket dataLayer . tiId) allTickets

    -- This is single-threaded.
    mapM_ processSingleTicket mZendeskResponses

    putTextLn "All the tickets has been processed."
  where
    -- | Process a single ticket after they were analyzed.
    processSingleTicket :: ZendeskResponse -> App ()
    processSingleTicket zendeskResponse = do

        -- We first fetch the function from the configuration
        printText           <- asksIOLayer iolPrintText
        appendF             <- asksIOLayer iolAppendFile -- We need to remove this.

        let ticketId = getTicketId $ zrTicketId zendeskResponse

        -- Printing id before inspecting the ticket so that when the process stops by the
        -- corrupted log file, we know which id to blacklist.
        printText $ "Analyzing ticket id: " <> show ticketId

        let tags = getTicketTags $ zrTags zendeskResponse
        forM_ tags $ \tag -> do
            let formattedTicketIdAndTag = show ticketId <> " " <> tag
            printText formattedTicketIdAndTag
            appendF "logs/analysis-result.log" (formattedTicketIdAndTag <> "\n")


-- | When we want to process all possible tickets.
processTickets :: HasCallStack => DataLayer App -> App ()
processTickets dataLayer = do

    printText <- asksIOLayer iolPrintText
    printText "Classifier will start processing tickets"

    -- Fetching all the tickets that needs to be processed
    allTickets <- fetchTickets dataLayer

    printText $ "Number of tickets to be analyzed: " <> show (length allTickets)

    mapM_ (processTicketSafe dataLayer . tiId) allTickets

    putTextLn "All the tickets has been processed."

-- | Fetch all tickets that needs to be analyzed
fetchTickets :: DataLayer App -> App [TicketInfo]
fetchTickets dataLayer = do
    sortedTicketIds             <- listAndSortTickets dataLayer
    sortedUnassignedTicketIds   <- listAndSortUnassignedTickets dataLayer

    let allTickets = sortedTicketIds <> sortedUnassignedTicketIds

    -- Anything that has a "to_be_analysed" tag
    pure $ filter (elem (renderTicketStatus ToBeAnalyzed) . getTicketTags . tiTags) allTickets

-- | Fetch a single ticket with given 'TicketId'
fetchTicket :: DataLayer App -> TicketId -> App (Maybe TicketInfo)
fetchTicket dataLayer ticketId = do
    let getTicketInfo       = zlGetTicketInfo dataLayer
    getTicketInfo ticketId


-- | Fetch comments with given 'TicketId'
fetchTicketComments :: DataLayer App -> TicketId -> App [Comment]
fetchTicketComments dataLayer ticketId = do
    let getTicketComments   = zlGetTicketComments dataLayer
    getTicketComments ticketId


-- | Fetch tickets that need to be analyzed and print them on console
fetchAndShowTickets :: DataLayer App -> App ()
fetchAndShowTickets dataLayer = do
    allTickets  <- fetchTickets dataLayer

    putTextLn $ "There are " <> show (length allTickets) <> " tickets."

    output      <- mapConcurrently (pure . show @Text) allTickets

    mapM_ putTextLn output

    putTextLn "All the tickets has been processed."

-- | Fetch and show tickets from a specific time.
fetchAndShowTicketsFrom :: DataLayer App -> ExportFromTime -> App ()
fetchAndShowTicketsFrom dataLayer exportFromTime = do
    allTickets <- fetchTicketsExportedFromTime dataLayer exportFromTime

    putTextLn $ "There are " <> show (length allTickets) <> " tickets."

    output      <- mapConcurrently (pure . show @Text) allTickets

    mapM_ putTextLn output

    putTextLn "All the tickets has been processed."

-- | List and sort tickets
-- TODO(ks): Extract repeating code, generalize.
listAndSortTickets :: HasCallStack => DataLayer App -> App [TicketInfo]
listAndSortTickets dataLayer = do

    Config{..}  <- ask

    let listAgents = zlListAdminAgents dataLayer
    agents      <- listAgents

    let agentIds :: [UserId]
        agentIds = map uId agents
    -- We first fetch the function from the configuration
    let listTickets = zlListAssignedTickets dataLayer

    ticketInfos <- map concat $ traverse listTickets agentIds

    let filteredTicketIds = filterAnalyzedTickets ticketInfos
    let sortedTicketIds   = sortBy compare filteredTicketIds

    pure sortedTicketIds

-- | Fetch tickets that are unassigned to Zendesk agents
listAndSortUnassignedTickets :: HasCallStack => DataLayer App -> App [TicketInfo]
listAndSortUnassignedTickets dataLayer = do

    -- We first fetch the function from the configuration
    let listUnassignedTickets = zlListUnassignedTickets dataLayer

    ticketInfos             <- listUnassignedTickets

    let filteredTicketIds   = filterAnalyzedTickets ticketInfos
    let sortedTicketIds     = sortBy compare filteredTicketIds

    pure sortedTicketIds

-- | A pure function for fetching 'Attachment' from 'Comment'
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

    -- For readability
    isAttachmentZip :: Attachment -> Bool
    isAttachmentZip attachment = aContentType attachment `elem`
        ["application/zip", "application/x-zip-compressed"]

-- | Inspects the comment, attachment, ticket info and create 'ZendeskResponse'
getZendeskResponses :: DataLayer App -> [Comment] -> [Attachment] -> TicketInfo -> App ZendeskResponse
getZendeskResponses dataLayer comments attachments ticketInfo
    | not (null attachments) = inspectAttachments dataLayer ticketInfo attachments
    | not (null comments)    = responseNoLogs ticketInfo
    | otherwise              = throwM $ CommentAndAttachmentNotFound (tiId ticketInfo)
    -- No attachment, no comments means something is wrong with ticket itself
  where
    -- Create 'ZendeskResponse' stating no logs were found on the ticket
    responseNoLogs :: TicketInfo -> App ZendeskResponse
    responseNoLogs TicketInfo{..} = do
        Config {..} <- ask
        pure ZendeskResponse
                { zrTicketId = tiId
                , zrComment  = prettyFormatNoLogs
                , zrTags     = TicketTags [renderTicketStatus NoLogAttached]
                , zrIsPublic = cfgIsCommentPublic
                }

-- | Inspect the latest attachment
inspectAttachments :: DataLayer App -> TicketInfo -> [Attachment] -> App ZendeskResponse
inspectAttachments dataLayer ticketInfo attachments = do

    config          <- ask
    let getAttachment   = zlGetAttachment dataLayer

    lastAttach      <- handleMaybe . safeHead . reverse . sort $ attachments
    att             <- handleMaybe =<< getAttachment lastAttach
    inspectAttachment config ticketInfo att
  where
    handleMaybe :: Maybe a -> App a
    handleMaybe Nothing  = throwM $ AttachmentNotFound (tiId ticketInfo)
    handleMaybe (Just a) = return a

-- | Inspection of the local zip.
-- This function prints out the analysis result on the console.
inspectLocalZipAttachment :: FilePath -> App ()
inspectLocalZipAttachment filePath = do

    config          <- ask
    printText       <- asksIOLayer iolPrintText

    -- Read the zip file
    fileContent     <- liftIO $ BS.readFile filePath
    let eResults = extractLogsFromZip 100 fileContent

    case eResults of
        Left (err :: ZipFileExceptions) ->
            printText $ show err
        Right result -> do
            let analysisEnv = setupAnalysis $ cfgKnowledgebase config
            eitherAnalysisResult    <- try $ extractIssuesFromLogs result analysisEnv

            case eitherAnalysisResult of
                Right analysisResult -> do
                    let errorCodes = extractErrorCodes analysisResult

                    printText "Analysis result:"
                    void $ mapM (printText . show) analysisResult

                    printText "Error codes:"
                    void $ mapM printText errorCodes

                Left (e :: LogAnalysisException) ->
                    printText $ show e

-- | Given number of file of inspect, knowledgebase and attachment,
-- analyze the logs and return the results.
inspectAttachment :: (MonadCatch m) => Config -> TicketInfo -> AttachmentContent -> m ZendeskResponse
inspectAttachment Config{..} ticketInfo@TicketInfo{..} attachment = do

    let analysisEnv = setupAnalysis cfgKnowledgebase
    let eLogFiles = extractLogsFromZip cfgNumOfLogsToAnalyze (getAttachmentContent attachment)

    case eLogFiles of
        Left _ ->
            -- Log file was corrupted
            pure $ ZendeskResponse
                { zrTicketId    = tiId
                , zrComment     = prettyFormatLogReadError ticketInfo
                , zrTags        = TicketTags [renderErrorCode SentLogCorrupted]
                , zrIsPublic    = cfgIsCommentPublic
                }

        Right logFiles -> do
            -- Log files maybe corrupted or issue was not found
            tryAnalysisResult    <- try $ extractIssuesFromLogs logFiles analysisEnv

            case tryAnalysisResult of
                Right analysisResult -> do
                    -- Known issue was found
                    let errorCodes = extractErrorCodes analysisResult
                    let commentRes = prettyFormatAnalysis analysisResult ticketInfo

                    pure $ ZendeskResponse
                        { zrTicketId    = tiId
                        , zrComment     = commentRes
                        , zrTags        = TicketTags errorCodes
                        , zrIsPublic    = cfgIsCommentPublic
                        }

                Left (analysisException :: LogAnalysisException) ->
                    case analysisException of
                        -- Could not read the log files
                        LogReadException ->
                            pure $ ZendeskResponse
                                { zrTicketId    = tiId
                                , zrComment     = prettyFormatLogReadError ticketInfo
                                , zrTags        = TicketTags [renderErrorCode DecompressionFailure]
                                , zrIsPublic    = cfgIsCommentPublic
                                }
                        -- No known issue was found
                        NoKnownIssueFound ->
                            pure $ ZendeskResponse
                                { zrTicketId    = tiId
                                , zrComment     = prettyFormatNoIssues ticketInfo
                                , zrTags        = TicketTags [renderTicketStatus NoKnownIssue]
                                , zrIsPublic    = cfgIsCommentPublic
                                }
                        JSONDecodeFailure errorText ->
                            throwM $ JSONDecodeFailure errorText

-- | Filter tickets
filterAnalyzedTickets :: [TicketInfo] -> [TicketInfo]
filterAnalyzedTickets ticketsInfo =
    filter ticketsFilter ticketsInfo
  where
    ticketsFilter :: TicketInfo -> Bool
    ticketsFilter ticketInfo =
           isTicketOpen ticketInfo
        && isTicketBlacklisted ticketInfo
        && isTicketInGoguenTestnet ticketInfo

    unsolvedTicketStatus :: [TicketStatus]
    unsolvedTicketStatus = map TicketStatus ["new", "open", "hold", "pending"]

    isTicketOpen :: TicketInfo -> Bool
    isTicketOpen TicketInfo{..} = tiStatus `elem` unsolvedTicketStatus

    -- | If we have a ticket we are having issues with...
    isTicketBlacklisted :: TicketInfo -> Bool
    isTicketBlacklisted TicketInfo{..} = tiId `notElem` [TicketId 9377, TicketId 10815, TicketId 15066, TicketId 30849]

    isTicketInGoguenTestnet :: TicketInfo -> Bool
    isTicketInGoguenTestnet TicketInfo{..} = "goguen_testnets" `notElem` getTicketTags tiTags
