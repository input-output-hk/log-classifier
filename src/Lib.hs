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
    , exportZendeskDataToLocalDB
    ) where

import           Universum

import           UnliftIO.Async (mapConcurrently)
import           UnliftIO.Concurrent (threadDelay)

import           Data.Attoparsec.Text.Lazy (eitherResult, parse)
import           Data.List (nub)
import           Data.Text (isInfixOf, stripEnd)
import qualified Data.ByteString.Lazy as BS

import           System.Directory (createDirectoryIfMissing)
import           System.IO (hSetBuffering, BufferMode (..))

import           CLI (CLI (..), getCliArgs)
import           DataSource (App, Attachment (..), AttachmentContent (..), Comment (..),
                             CommentBody (..), Config (..), DBLayer (..), DeletedTicket (..),
                             ExportFromTime (..), IOLayer (..), TicketId (..), TicketInfo (..),
                             TicketStatus (..), TicketTag (..), TicketTags (..), User (..),
                             UserId (..), ZendeskLayer (..), ZendeskResponse (..), asksDBLayer,
                             asksIOLayer, asksZendeskLayer, assignToPath, connPoolDBLayer,
                             createProdConnectionPool, defaultConfig, knowledgebasePath,
                             renderTicketStatus, runApp, tokenPath)
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
    hSetBuffering stdout NoBuffering

    args        <- getCliArgs

    putTextLn "Welcome to Zendesk classifier!"
    token       <- readFile tokenPath       -- Zendesk token
    assignFile  <- readFile assignToPath    -- Select assignee
    knowledges  <- setupKnowledgebaseEnv knowledgebasePath
    assignTo    <- case readEither assignFile of
        Right agentid -> return agentid
        Left  err     -> error err

    connPool    <- createProdConnectionPool

    let cfg = defaultConfig
                   { cfgToken         = stripEnd token
                   , cfgAssignTo      = assignTo
                   , cfgAgentId       = assignTo
                   , cfgKnowledgebase = knowledges
                   , cfgDBLayer       = connPoolDBLayer connPool
                   }

    -- At this point, the configuration is set up and there is no point in using a pure IO.
    case args of
        CollectEmails                   -> runApp collectEmails cfg
        FetchAgents                     -> void $ runApp fetchAgents cfg
        FetchTickets                    -> runApp fetchAndShowTickets cfg
        FetchTicketsFromTime fromTime   -> runApp (fetchAndShowTicketsFrom fromTime) cfg
        (ProcessTicket ticketId)        -> void $ runApp (processTicket (TicketId ticketId)) cfg
        ProcessTickets                  -> void $ runApp processTickets cfg
        ProcessTicketsFromTime fromTime -> runApp (processTicketsFromTime fromTime) cfg
        ShowStatistics                  -> void $ runApp (fetchTickets >>= showStatistics) cfg
        InspectLocalZip filePath        -> runApp (inspectLocalZipAttachment filePath) cfg
        ExportData fromTime             -> void $ runApp (exportZendeskDataToLocalDB fromTime) cfg

-- | The function for exporting Zendesk data to our local DB so
-- we can have faster analysis and runs.
-- We expect that the local database exists and has the correct schema.
exportZendeskDataToLocalDB :: ExportFromTime -> App [TicketInfo]
exportZendeskDataToLocalDB exportFromTime = do

    deleteAllData               <- asksDBLayer dlDeleteAllData

    notDeletedExportedTickets   <- fetchTicketsExportedFromTime exportFromTime

    -- We want to call in parallel 400 HTTP requests to fetch the data.
    let chunkedExportedTickets = chunks 400 notDeletedExportedTickets

    -- The max requests are 400 per minute, so we wait a minute!
    ticketData <- forM chunkedExportedTickets $ \chunkedTickets -> do
        -- Wait a minute!
        threadDelay $ 61 * 1000000
        -- Concurrently we fetch the ticket data. If a single
        -- call fails, they all fail. When they all finish, they return the
        -- result.
        mapConcurrently fetchTicketData chunkedTickets

    let allTicketData = concat ticketData

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

        getTicketComments           <- asksZendeskLayer zlGetTicketComments

        -- First fetch comments, FAIL-FAST
        ticketComments              <- getTicketComments $ tiId ticket

        pure (ticket, ticketComments)

-- | Fetch all tickets changed from a specific date.
fetchTicketsExportedFromTime :: ExportFromTime -> App [TicketInfo]
fetchTicketsExportedFromTime exportFromTime = do

    listDeletedTickets          <- asksZendeskLayer zlListDeletedTickets
    exportTickets               <- asksZendeskLayer zlExportTickets

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

-- | So we don't have to include another library here.
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

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

-- | When we want to process a specific ticket.
processTicket :: TicketId -> App (Maybe ZendeskResponse)
processTicket tId = do

    -- We see 3 HTTP calls here.
    getTicketInfo       <- asksZendeskLayer zlGetTicketInfo
    getTicketComments   <- asksZendeskLayer zlGetTicketComments
    postTicketComment   <- asksZendeskLayer zlPostTicketComment

    mTicketInfo         <- getTicketInfo tId
    comments            <- getTicketComments tId

    let attachments     = getAttachmentsFromComment comments
    let ticketInfo      = fromMaybe (error "No ticket info") mTicketInfo

    zendeskResponse     <- getZendeskResponses comments attachments ticketInfo

    whenJust zendeskResponse $ \response -> do
        postTicketComment ticketInfo response

    pure zendeskResponse

-- | When we want to process all tickets from a specific time onwards.
-- Run in parallel.
processTicketsFromTime :: ExportFromTime -> App ()
processTicketsFromTime exportFromTime = do

    allTickets          <- fetchTicketsExportedFromTime exportFromTime

    let chunkedTickets = chunks 100 allTickets

    -- The max requests are 400 per minute, so we wait a minute!
    mZendeskResponses <- forM chunkedTickets $ \chunkedTickets' -> do
        -- Wait a minute!
        threadDelay $ 61 * 1000000
        -- Concurrently we fetch the ticket data. If a single
        -- call fails, they all fail. When they all finish, they return the
        -- result.
        mapConcurrently (processTicket . tiId) chunkedTickets'

    -- This is single-threaded.
    mapM_ processSingleTicket $ concat mZendeskResponses

    putTextLn "All the tickets has been processed."
  where
    -- | Process a single ticket after they were analyzed.
    processSingleTicket :: Maybe ZendeskResponse -> App ()
    processSingleTicket zendeskResponse = do

        -- We first fetch the function from the configuration
        printText           <- asksIOLayer iolPrintText
        appendF             <- asksIOLayer iolAppendFile -- We need to remove this.

        whenJust zendeskResponse $ \response -> do
            let ticketId = getTicketId $ zrTicketId response

            -- Printing id before inspecting the ticket so that when the process stops by the
            -- corrupted log file, we know which id to blacklist.
            printText $ "Analyzing ticket id: " <> show ticketId

            let tags = getTicketTags $ zrTags response
            forM_ tags $ \tag -> do
                let formattedTicketIdAndTag = show ticketId <> " " <> tag
                printText formattedTicketIdAndTag
                appendF "logs/analysis-result.log" (formattedTicketIdAndTag <> "\n")


-- | When we want to process all possible tickets.
processTickets :: App ()
processTickets = do

    allTickets          <- fetchTickets
    _                   <- mapM (processTicket . tiId) allTickets

    putTextLn "All the tickets has been processed."

-- | Fetch all tickets.
fetchTickets :: App [TicketInfo]
fetchTickets = do
    sortedTicketIds             <- listAndSortTickets
    sortedUnassignedTicketIds   <- listAndSortUnassignedTickets

    let allTickets = sortedTicketIds <> sortedUnassignedTicketIds
    return allTickets

fetchAndShowTickets :: App ()
fetchAndShowTickets = do
    allTickets <- fetchTickets

    -- We want to call in parallel 400 HTTP requests to fetch the data. 3 calls are required.
    let chunkedTickets = chunks 100 allTickets

    -- The max requests are 400 per minute, so we wait a minute!
    output <- forM chunkedTickets $ \chunkedTickets' -> do
        -- Concurrently we fetch the ticket data. If a single
        -- call fails, they all fail. When they all finish, they return the
        -- result.
        mapConcurrently (pure . show @Text) chunkedTickets'

    mapM_ putTextLn (concat output)

    putTextLn "All the tickets has been processed."

-- | Fetch and show tickets from a specific time.
fetchAndShowTicketsFrom :: ExportFromTime -> App ()
fetchAndShowTicketsFrom exportFromTime = do
    allTickets <- fetchTicketsExportedFromTime exportFromTime

    let chunkedTickets = chunks 100 allTickets

    -- The max requests are 400 per minute, so we wait a minute!
    output <- forM chunkedTickets $ \chunkedTickets' -> do
        -- Concurrently we fetch the ticket data. If a single
        -- call fails, they all fail. When they all finish, they return the
        -- result.
        mapConcurrently (pure . show . tiId) chunkedTickets'

    mapM_ putTextLn (concat output)

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

-- | Inspection of the local zip.
-- This function prints out the analysis result on the console.
inspectLocalZipAttachment :: FilePath -> App ()
inspectLocalZipAttachment filePath = do

    config          <- ask
    printText       <- asksIOLayer iolPrintText

    -- Read the zip file
    fileContent     <- liftIO $ BS.readFile filePath
    let results     = extractLogsFromZip 100 fileContent

    case results of
        Left err -> do
            printText err
        Right result -> do
            let analysisEnv             = setupAnalysis $ cfgKnowledgebase config
            let eitherAnalysisResult    = extractIssuesFromLogs result analysisEnv

            case eitherAnalysisResult of
                Right analysisResult -> do
                    let errorCodes = extractErrorCodes analysisResult

                    printText "Analysis result:"
                    void $ mapM (printText . show) analysisResult

                    printText "Error codes:"
                    void $ mapM printText errorCodes

                Left e -> do
                    printText e

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
    analyzedTags = map renderTicketStatus [AnalyzedByScriptV1_0, AnalyzedByScriptV1_1, AnalyzedByScriptV1_2]

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
