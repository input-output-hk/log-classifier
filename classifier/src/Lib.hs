{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
    ( createBasicDataLayerIO
    , createConfig
    -- * library functions
    , getZendeskResponses
    , getAttachmentsFromComment
    , inspectAttachment
    , ExtractLogFileFunc
    , ExtractErrorCodeFunc
    , processTicket
    , processTicketSafe
    , processTickets
    , fetchTickets
    , showStatistics
    , filterAnalyzedTickets
    , exportZendeskDataToLocalDB
    -- * Optional
    , fetchTicket
    , fetchTicketComments
    , fetchAgents
    , fetchAndShowTickets
    , fetchAndShowTicketsFrom
    , inspectLocalZipAttachment
    , processTicketsFromTime
    ) where

import           Universum

import           UnliftIO.Async (mapConcurrently)

import           Control.Exception.Safe (Handler (..), catches)
import           Data.Attoparsec.Text.Lazy (eitherResult, parse)
import qualified Data.ByteString.Lazy as BS
import           Data.List (nub)
import           Data.Text (stripEnd)

import           System.Directory (createDirectoryIfMissing)
import           System.IO (BufferMode (..), hSetBuffering)

import           Configuration (defaultConfig)
import           DataSource (App, Attachment (..), AttachmentContent (..), Comment (..),
                             Config (..), DBLayer (..), DataLayer (..), DeletedTicket (..),
                             ExportFromTime (..), IOLayer (..), TicketId (..), TicketInfo (..),
                             TicketStatus (..), TicketTag (..), TicketTags (..), User (..),
                             UserId (..), ZendeskResponse (..), asksDBLayer, asksIOLayer,
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
import           LogAnalysis.Types (Analysis, ErrorCode (..), Knowledge, LogFile, renderErrorCode,
                                    setupAnalysis)
import           Statistics (showStatistics)
import           Util (extractLogsFromZip)

------------------------------------------------------------
-- Functions
------------------------------------------------------------

-- | Create configuration.
createConfig :: IO Config
createConfig = do
    createDirectoryIfMissing True "logs"
    hSetBuffering stdout NoBuffering

    putTextLn "Welcome to Zendesk classifier!"
    token       <- readFile tokenPath       -- Zendesk token
    assignFile  <- readFile assignToPath    -- Select assignee
    knowledges  <- setupKnowledgebaseEnv knowledgebasePath
    assignTo    <- case readEither assignFile of
        Right agentid -> return agentid
        Left  _       -> throwM FailedToReadAssignFile

    connPool    <- createProdConnectionPool

    pure defaultConfig
        { cfgToken         = stripEnd token
        , cfgAssignTo      = assignTo
        , cfgAgentId       = UserId (fromIntegral assignTo)
        , cfgKnowledgebase = knowledges
        , cfgDBLayer       = connPoolDBLayer connPool
        }

  where
    -- Read CSV file and setup knowledge base
    setupKnowledgebaseEnv :: FilePath -> IO [Knowledge]
    setupKnowledgebaseEnv path = do
        kfile <- toLText <$> readFile path
        let kb = parse parseKnowLedgeBase kfile
        case eitherResult kb of
            Left _   -> throwM FailedToParseKnowledgebase
            Right ks -> return ks

-- | Create basic data layer. Do this ONCE!
createBasicDataLayerIO :: Config -> IO (DataLayer App)
createBasicDataLayerIO config = runApp createBasicDataLayer config
  where
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

-- | 'processTicket' with exception handling
processTicketSafe :: DataLayer App -> TicketId -> App ()
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
    --let postTicketComment   = zlPostTicketComment dataLayer

    mTicketInfo         <- getTicketInfo tId
    comments            <- getTicketComments tId

    let attachments     = getAttachmentsFromComment comments
    case mTicketInfo of
        Nothing         -> throwM $ TicketInfoNotFound tId
        Just ticketInfo -> do
            zendeskResponse <- getZendeskResponses dataLayer comments attachments ticketInfo

            -- post ticket comment
            -- Maybe for now we don't need to actually post this, but let the agent post it.
           -- postTicketComment ticketInfo zendeskResponse
           -- print zendeskResponse

            pure zendeskResponse

-- | When we want to process all possible tickets.
processTickets :: HasCallStack => DataLayer App -> App [ZendeskResponse]
processTickets dataLayer = do

    printText <- asksIOLayer iolPrintText
    printText "Classifier will start processing tickets"

    -- Fetching all the tickets that needs to be processed
    allTickets <- fetchTickets dataLayer

    printText $ "Number of tickets to be analyzed: " <> show (length allTickets)

    zendeskResponses <- mapM (processTicket dataLayer . tiId) allTickets

    putTextLn "All the tickets has been processed."

    pure zendeskResponses

-- | Fetch all tickets that needs to be analyzed
fetchTickets :: DataLayer App -> App [TicketInfo]
fetchTickets dataLayer = do
    allTickets <- zlListToBeAnalysedTickets dataLayer

    let filteredTickets = sortBy compare $ filterAnalyzedTickets allTickets
    -- Any ticket that has a "to_be_analysed" tag
    pure filteredTickets

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
    inspectAttachment config ticketInfo att extractLogsFromZip extractIssuesFromLogs
  where
    handleMaybe :: Maybe a -> App a
    handleMaybe Nothing  = throwM $ AttachmentNotFound (tiId ticketInfo)
    handleMaybe (Just a) = return a

-- This is to enable testing on 'inspectAttachment'
type ExtractLogFileFunc = Int -> LByteString -> Either ZipFileExceptions [LogFile]
type ExtractErrorCodeFunc m = [LogFile] -> Analysis -> m Analysis

-- | Given number of file of inspect, knowledgebase and attachment,
-- analyze the logs and return the results.
inspectAttachment :: (MonadCatch m)
                  => Config
                  -> TicketInfo
                  -> AttachmentContent
                  -> ExtractLogFileFunc
                  -> ExtractErrorCodeFunc m
                  -> m ZendeskResponse
inspectAttachment Config{..} ticketInfo attachment extractLogFileFunc extractIssueFunc =
    flip catches [Handler handleZipFileException, Handler handleLogAnalysisException] $ do

        let analysisEnv = setupAnalysis cfgKnowledgebase
        -- let logFiles    = either throwM id $ extractLogFileFunc cfgNumOfLogsToAnalyze
        --                 $ getAttachmentContent attachment
        logFiles <- either throwM pure $
           extractLogFileFunc cfgNumOfLogsToAnalyze $ getAttachmentContent attachment

        -- Perform analysis
        analysisResult <- extractIssueFunc logFiles analysisEnv
        let errorCodes  = extractErrorCodes analysisResult
        let commentRes  = prettyFormatAnalysis analysisResult ticketInfo

        pure $ mkZendeskResponse commentRes errorCodes
   where
     handleZipFileException :: (Monad m) => ZipFileExceptions -> m ZendeskResponse
     handleZipFileException _ =
        pure $ mkZendeskErrorResponse (prettyFormatLogReadError ticketInfo) SentLogCorrupted

     handleLogAnalysisException :: (MonadThrow m) => LogAnalysisException -> m ZendeskResponse
     handleLogAnalysisException = \case
        LogReadException ->
            pure $ mkZendeskErrorResponse (prettyFormatLogReadError ticketInfo) SentLogCorrupted

        NoKnownIssueFound ->
            pure $ mkZendeskErrorResponse (prettyFormatNoIssues ticketInfo) NoKnownIssue

        (JSONDecodeFailure errorText) -> throwM $ JSONDecodeFailure errorText

     mkZendeskResponse :: Text -> [Text] -> ZendeskResponse
     mkZendeskResponse comment errorCodes = ZendeskResponse
        { zrTicketId = tiId ticketInfo
        , zrComment  = comment
        , zrTags     = TicketTags errorCodes
        , zrIsPublic = cfgIsCommentPublic
        }

     mkZendeskErrorResponse :: Text -> ErrorCode -> ZendeskResponse
     mkZendeskErrorResponse comment errorCode = mkZendeskResponse comment [renderErrorCode errorCode]

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

------------------------------------------------------------
-- Utility functions
------------------------------------------------------------

fetchAgents :: DataLayer App -> App [User]
fetchAgents dataLayer = do
    Config{..}          <- ask

    let listAdminAgents = zlListAdminAgents dataLayer
    printText           <- asksIOLayer iolPrintText

    printText "Fetching Zendesk agents"

    agents              <- listAdminAgents

    mapM_ print agents
    pure agents

-- | Fetch tickets that need to be analyzed and print them on console
fetchAndShowTickets :: DataLayer App -> App ()
fetchAndShowTickets dataLayer = do
    allTickets <- fetchTickets dataLayer

    showTickets allTickets

-- | Fetch and show tickets from a specific time.
fetchAndShowTicketsFrom :: DataLayer App -> ExportFromTime -> App ()
fetchAndShowTicketsFrom dataLayer exportFromTime = do
    allTickets <- fetchTicketsExportedFromTime dataLayer exportFromTime

    showTickets allTickets

-- | Display all the TicketId and its tags
showTickets :: [TicketInfo] -> App ()
showTickets tickets = do
    putTextLn $ "There are " <> show (length tickets) <> " tickets."

    forM_ tickets $ \ticket -> do
        let ticketId   = getTicketId $ tiId ticket
        let ticketTags = (sort . getTicketTags . tiTags) ticket
        putTextLn $ "TicketId: " <> show ticketId <> ", Tags: " <> show ticketTags

-- | Inspection of the local zip.
-- This function prints out the analysis result on the console.
-- Can apply same refactoring as 'inspectAttachment'
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
