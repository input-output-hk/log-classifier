{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Zendesk
    ( runZendeskMain
    ) where

import           Universum

import           Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import           Data.Aeson (FromJSON, ToJSON, Value, encode, parseJSON)
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Aeson.Types (Parser, parseEither)
import           Data.Attoparsec.Text.Lazy (eitherResult, parse)
import           Data.Text (isInfixOf, stripEnd)
import           Network.HTTP.Simple (Request, addRequestHeader, getResponseBody, httpJSON, httpLBS,
                                      parseRequest_, setRequestBasicAuth, setRequestBodyJSON,
                                      setRequestMethod, setRequestPath)

import           CLI (CLI (..), getCliArgs)
import           LogAnalysis.Classifier (extractErrorCodes, extractIssuesFromLogs,
                                         prettyFormatAnalysis, prettyFormatLogReadError,
                                         prettyFormatNoIssues)
import           LogAnalysis.KnowledgeCSVParser (parseKnowLedgeBase)
import           LogAnalysis.Types (ErrorCode (..), Knowledge, renderErrorCode, setupAnalysis)
import           Types (Attachment (..), Comment (..), Ticket (..), TicketId, TicketInfo (..),
                        TicketList (..), TicketTag (..), ZendeskResponse (..), parseAgentId,
                        parseComments, parseTickets, renderTicketStatus)
import           Util (extractLogsFromZip)

-- TODO: Better exception handling

data Config = Config
    { cfgAgentId            :: !Integer
    -- ^ Zendesk agent id
    , cfgZendesk            :: !Text
    -- ^ URL to Zendesk
    , cfgToken              :: !Text
    -- ^ Zendesk token
    , cfgEmail              :: !Text
    -- ^ Email address of the user the classifier will process on
    , cfgAssignTo           :: !Integer
    -- ^ User that will be assigned to after the classifier has done the analysis
    , cfgKnowledgebase      :: ![Knowledge]
    -- ^ Knowledgebase
    , cfgNumOfLogsToAnalyze :: !Int
    -- ^ Number of files classifier will analyze
    } deriving (Eq, Show)

data RequestType
    = Requested
    | Assigned

newtype App a = App (ReaderT Config IO a)
    deriving ( Applicative
             , Functor
             , Monad
             , MonadReader Config
             , MonadIO
             )

runApp :: App a -> Config -> IO a
runApp (App a) = runReaderT a

defaultConfig :: Config
defaultConfig = Config 0 "https://iohk.zendesk.com" "" "daedalus-bug-reports@iohk.io" 0 [] 5

-- | Path to knowledgebase
knowledgebasePath :: FilePath
knowledgebasePath = "./knowledgebase/knowledge.csv"

-- | Filepath to token file
tokenPath :: FilePath
tokenPath = "./tmp-secrets/token"

-- | Filepath to assign_to file
assignToPath :: FilePath
assignToPath = "./tmp-secrets/assign_to"

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
    agentId <- runApp getAgentId cfg'
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
        -- Return raw request
        (RawRequest url) -> do
            let req = apiRequest cfg (toText url)
            res <- apiCall (pure . encodeToLazyText)  req
            putTextLn (toText res)
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
    liftIO $ putTextLn $ "Analyzing ticket id: " <> show ticketId
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
inspectAttachment TicketInfo{..} att = do
    Config{..} <- ask

    rawlog <- liftIO $ getAttachment att   -- Get attachment
    let results = extractLogsFromZip cfgNumOfLogsToAnalyze rawlog

    case results of
        Left err -> do
            liftIO $ putTextLn $ "Error parsing zip:" <> err
            pure ZendeskResponse
                { zrComment     = prettyFormatLogReadError ticketUrl
                , zrTags        = [renderErrorCode SentLogCorrupted]
                , zrIsPublic    = False
                }
        Right result -> do
            let analysisEnv             = setupAnalysis cfgKnowledgebase
            let eitherAnalysisResult    = extractIssuesFromLogs result analysisEnv

            case eitherAnalysisResult of
                Right analysisResult -> do
                    let errorCodes = extractErrorCodes analysisResult
                    let commentRes = prettyFormatAnalysis analysisResult ticketUrl

                    liftIO $ mapM_ putTextLn errorCodes

                    pure ZendeskResponse
                        { zrComment     = commentRes
                        , zrTags        = errorCodes
                        , zrIsPublic    = False
                        }

                Left noResult -> do
                    liftIO $ putStrLn noResult
                    pure ZendeskResponse
                        { zrComment     = prettyFormatNoIssues ticketUrl
                        , zrTags        = [renderTicketStatus NoKnownIssue]
                        , zrIsPublic    = False
                        }

-- | Filter analyzed tickets
filterAnalyzedTickets :: [TicketInfo] -> [TicketInfo]
filterAnalyzedTickets ticketsInfo =
    filter ticketsFilter ticketsInfo
  where
    ticketsFilter :: TicketInfo -> Bool
    ticketsFilter ticketInfo =
        isTicketAnalyzed ticketInfo && isTicketOpen ticketInfo

    isTicketAnalyzed :: TicketInfo -> Bool
    isTicketAnalyzed TicketInfo{..} = (renderTicketStatus AnalyzedByScript) `notElem` ticketTags

    isTicketOpen :: TicketInfo -> Bool
    isTicketOpen TicketInfo{..} = ticketStatus == "open" -- || ticketStatus == "new"


-- | Get single ticket info.
getTicketInfo :: TicketId -> App TicketInfo
getTicketInfo ticketId = do
    cfg <- ask

    let req = apiRequest cfg ("tickets/" <> show ticketId <> ".json")
    liftIO $ apiCall parseJSON req

-- | Return list of ticketIds that has been requested by config user (not used)
listTickets :: RequestType ->  App [TicketInfo]
listTickets request = do
    cfg <- ask

    let agentId = cfgAgentId cfg
    let url = case request of
                  Requested -> "/users/" <> show agentId <> "/tickets/requested.json"
                  Assigned  -> "/users/" <> show agentId <> "/tickets/assigned.json"
    let req = apiRequest cfg url

    let go :: [TicketInfo] -> Text -> IO [TicketInfo]
        go list' nextPage' = do
          let req' = apiRequestAbsolute cfg nextPage'
          (TicketList pagen nextPagen) <- apiCall parseTickets req'
          case nextPagen of
              Just nextUrl -> go (list' <> pagen) nextUrl
              Nothing      -> pure (list' <> pagen)

    (TicketList page0 nextPage) <- liftIO $ apiCall parseTickets req
    case nextPage of
        Just nextUrl -> liftIO $ go page0 nextUrl
        Nothing      -> pure page0

-- | Send API request to post comment
postTicketComment :: TicketId -> Text -> [Text] -> Bool -> App ()
postTicketComment tid body tags public = do
    cfg <- ask
    let req1 = apiRequest cfg ("tickets/" <> show tid <> ".json")
    let req2 = addJsonBody
                   (Ticket
                       (Comment ("**Log classifier**\n\n" <> body) [] public (cfgAgentId cfg))
                       (cfgAssignTo cfg)
                       (renderTicketStatus AnalyzedByScript:tags)
                   )
                   req1
    void $ liftIO $ apiCall (pure . encodeToLazyText) req2
    pure ()

-- | Get agent id that has been set on Config
getAgentId :: App Integer
getAgentId = do
    cfg <- ask
    let req = apiRequest cfg "users/me.json"
    liftIO $ apiCall parseAgentId req

-- | Given attachmentUrl, return attachment in bytestring
getAttachment :: Attachment -> IO LByteString
getAttachment Attachment{..} = getResponseBody <$> httpLBS req
    where
      req :: Request
      req = parseRequest_ (toString aURL)

-- | Get ticket's comments
getTicketComments :: TicketId -> App [Comment]
getTicketComments tid = do
    cfg <- ask
    let req = apiRequest cfg ("tickets/" <> show tid <> "/comments.json")
    liftIO $ apiCall parseComments req

-- | Request PUT
addJsonBody :: ToJSON a => a -> Request -> Request
addJsonBody body req = setRequestBodyJSON body $ setRequestMethod "PUT" req

-- | Make an api call
apiCall :: FromJSON a => (Value -> Parser a) -> Request -> IO a
apiCall parser req = do
    v <- getResponseBody <$> httpJSON req
    case parseEither parser v of
        Right o -> pure o
        Left e -> error $ "couldn't parse response "
            <> toText e <> "\n" <> decodeUtf8 (encode v)

-- | General api request function
apiRequest :: Config -> Text -> Request
apiRequest Config{..} u = setRequestPath (encodeUtf8 path) $
                          addRequestHeader "Content-Type" "application/json" $
                          setRequestBasicAuth
                              (encodeUtf8 cfgEmail <> "/token")
                              (encodeUtf8 cfgToken) $
                          parseRequest_ (toString (cfgZendesk <> path))
                        where
                          path :: Text
                          path = "/api/v2/" <> u

-- | Api request but use absolute path
apiRequestAbsolute :: Config -> Text -> Request
apiRequestAbsolute Config{..} u = addRequestHeader "Content-Type" "application/json" $
                                  setRequestBasicAuth
                                      (encodeUtf8 cfgEmail <> "/token")
                                      (encodeUtf8 cfgToken) $
                                  parseRequest_ (toString u)
