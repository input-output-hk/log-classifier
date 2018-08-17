module Main where

import           Universum

import           Data.List (nub)
import           Database.SQLite.Simple (Connection (..), NamedParam (..), queryNamed,
                                         withConnection, query_)

import           Test.Hspec (Spec, describe, hspec, it, pending, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Gen, arbitrary, elements, forAll, listOf, listOf1, property,
                                  (===))
import           Test.QuickCheck.Monadic (assert, monadicIO, pre, run)

import           Configuration (basicIOLayer, defaultConfig)
import           DataSource (App, Attachment (..), AttachmentId (..), Comment (..),
                             CommentBody (..), CommentId (..), Config (..), DBLayerException (..),
                             DataLayer (..), DeletedTicket (..), ExportFromTime (..), IOLayer (..),
                             Ticket (..), TicketId (..), TicketInfo (..), TicketStatus (..),
                             TicketTag (..), TicketTags (..), TicketURL (..), User, UserId (..),
                             ZendeskAPIUrl (..), ZendeskResponse (..), createResponseTicket,
                             createSchema, deleteAllData, emptyDBLayer, emptyDataLayer,
                             insertCommentAttachments, insertTicketComments, insertTicketInfo,
                             renderTicketStatus, runApp, showURL)
import           Exceptions (ProcessTicketExceptions (..))

import           Lib (exportZendeskDataToLocalDB, filterAnalyzedTickets, getAttachmentsFromComment,
                      listAndSortTickets, processTicket)
import           Statistics (filterTicketsByStatus, filterTicketsWithAttachments,
                             showAttachmentInfo, showCommentAttachments)

-- TODO(ks): What we are really missing is a realistic @Gen DataLayer m@.

main :: IO ()
main = hspec spec

-- stack test log-classifier --fast --test-arguments "-m Zendesk"
spec :: Spec
spec =
    describe "Log Classifier Tests" $ do
        describe "Statistics" $ do
            filterTicketsWithAttachmentsSpec
            filterTicketsByStatusSpec
            showAttachmentInfoSpec
            showCommentAttachmentsSpec
            showTicketCategoryCountSpec
            -- TODO(rc): showTicketWithAttachmentsSpec
            -- TODO(rc): showTicketAttachmentsSpec
            -- TODO(rc): showStatisticsSpec
        describe "Zendesk" $ do
            validShowURLSpec
            listAndSortTicketsSpec
            processTicketSpec
            filterAnalyzedTicketsSpec
            createResponseTicketSpec
            exportZendeskDataToLocalDBSpec
            getAttachmentsFromCommentSpec

        describe "Database" $ do
            deleteAllDataSpec
            insertCommentAttachmentsSpec
            insertTicketInfoSpec
            insertTicketCommentsSpec

-- | A utility function for testing which stubs IO and returns
-- the @Config@ with the @DataLayer@ that was passed into it.
withStubbedIOAndDataLayer :: DataLayer App -> Config
withStubbedIOAndDataLayer stubbedDataLayer =
    defaultConfig
        { cfgDataLayer   = stubbedDataLayer
        , cfgIOLayer        = stubbedIOLayer
        , cfgDBLayer        = emptyDBLayer
        }
  where
    stubbedIOLayer :: IOLayer App
    stubbedIOLayer =
        basicIOLayer
            { iolPrintText      = \_     -> pure ()
            , iolAppendFile     = \_ _   -> pure ()
            -- ^ Do nothing with the output
            }

listAndSortTicketsSpec :: Spec
listAndSortTicketsSpec =
    describe "listAndSortTickets" $ modifyMaxSuccess (const 200) $ do
        it "doesn't return tickets since there are none" $
            forAll arbitrary $ \(ticketInfo :: TicketInfo) ->

                monadicIO $ do

                    let stubbedDataLayer :: DataLayer App
                        stubbedDataLayer =
                            emptyDataLayer
                                { zlListAssignedTickets     = \_     -> pure []
                                , zlGetTicketInfo           = \_     -> pure $ Just ticketInfo
                                , zlListAdminAgents         =           pure []
                                }

                    let stubbedConfig :: Config
                        stubbedConfig = withStubbedIOAndDataLayer stubbedDataLayer

                    let appExecution :: IO [TicketInfo]
                        appExecution = runApp listAndSortTickets stubbedConfig

                    tickets <- run appExecution

                    assert $ length tickets == 0

        it "returns sorted nonempty tickets" $
            forAll arbitrary $ \(ticketInfo) ->
                forAll (listOf1 arbitrary) $ \(listTickets) ->
                    forAll (listOf1 arbitrary) $ \(agents :: [User]) ->

                        monadicIO $ do

                            pre $ any (\TicketInfo{..} -> tiStatus /= TicketStatus "solved") listTickets

                            let stubbedDataLayer :: DataLayer App
                                stubbedDataLayer =
                                    emptyDataLayer
                                        { zlListAssignedTickets     = \_     -> pure listTickets
                                        , zlGetTicketInfo           = \_     -> pure ticketInfo
                                        , zlListAdminAgents         =           pure agents
                                        }

                            let stubbedConfig :: Config
                                stubbedConfig = withStubbedIOAndDataLayer stubbedDataLayer

                            let appExecution :: IO [TicketInfo]
                                appExecution = runApp listAndSortTickets stubbedConfig

                            tickets <- run appExecution

                            -- Check we have some tickets.
                            assert $ length tickets > 0
                            -- Check the order is sorted.
                            assert $ sortBy compare tickets == tickets

processTicketSpec :: Spec
processTicketSpec =
    describe "processTicket" $ modifyMaxSuccess (const 200) $ do
        it "processes ticket, with comments" $
            forAll arbitrary $ \(ticketInfo :: TicketInfo) ->
                forAll (listOf1 arbitrary) $ \(listTickets) ->
                forAll (listOf1 arbitrary) $ \(comments) ->

                    monadicIO $ do

                        let stubbedDataLayer :: DataLayer App
                            stubbedDataLayer =
                                emptyDataLayer
                                    { zlListAssignedTickets     = \_     -> pure listTickets
                                    , zlGetTicketInfo           = \_     -> pure $ Just ticketInfo
                                    , zlPostTicketComment       = \_ _   -> pure ()
                                    , zlGetTicketComments       = \_     -> pure comments
                                    , zlGetAttachment           = \_     -> pure $ Just mempty
                                    }

                        let stubbedConfig :: Config
                            stubbedConfig = withStubbedIOAndDataLayer stubbedDataLayer

                        let appExecution :: IO ZendeskResponse
                            appExecution = runApp (processTicket . tiId $ ticketInfo) stubbedConfig

                        zendeskResponse <- run appExecution

                        -- Check we have some comments.
                        assert . not . null . zrComment $ zendeskResponse

        it "processes ticket, with no attachments" $
            forAll (listOf1 genCommentWithNoAttachment) $ \(commentsWithoutAttachment :: [Comment]) ->
                forAll arbitrary $ \(ticketInfo :: TicketInfo) ->
                    monadicIO $ do

                    let stubbedDataLayer :: DataLayer App
                        stubbedDataLayer =
                            emptyDataLayer
                                { zlGetTicketComments = \_   -> pure commentsWithoutAttachment
                                , zlGetTicketInfo     = \_   -> pure $ Just ticketInfo
                                , zlPostTicketComment = \_ _ -> pure ()
                                }

                    let stubbedConfig :: Config
                        stubbedConfig = withStubbedIOAndDataLayer stubbedDataLayer

                    let appExecution :: IO ZendeskResponse
                        appExecution = runApp (processTicket . tiId $ ticketInfo) stubbedConfig

                    zendeskResponse <- run appExecution

                    assert . not . null . zrComment $ zendeskResponse
                    assert $ isResponseTaggedWithNoLogs zendeskResponse

        it "processes ticket, with attachments" $
            forAll (listOf1 arbitrary) $ \(comments :: [Comment]) ->
                forAll arbitrary $ \(ticketInfo :: TicketInfo) ->
                    monadicIO $ do

                    pre $ any (not . null . cAttachments) comments

                    let stubbedDataLayer :: DataLayer App
                        stubbedDataLayer =
                            emptyDataLayer
                                { zlGetTicketComments = \_   -> pure comments
                                , zlGetTicketInfo     = \_   -> pure $ Just ticketInfo
                                , zlPostTicketComment = \_ _ -> pure ()
                                , zlGetAttachment     = \_   -> pure $ Just mempty
                                }

                    let stubbedConfig :: Config
                        stubbedConfig = withStubbedIOAndDataLayer stubbedDataLayer

                    let appExecution :: IO ZendeskResponse
                        appExecution = runApp (processTicket . tiId $ ticketInfo) stubbedConfig

                    zendeskResponse <- run appExecution

                    assert . not . null . zrComment $ zendeskResponse
                    assert . not . isResponseTaggedWithNoLogs $ zendeskResponse

        it "tries to process ticket but cannot find both comments and attachments, throws exception" $
            forAll arbitrary $ \(ticketInfo :: TicketInfo) ->

                monadicIO $ do

                    let stubbedDataLayer :: DataLayer App
                        stubbedDataLayer =
                            emptyDataLayer
                                { zlGetTicketInfo           = \_     -> pure $ Just ticketInfo
                                , zlPostTicketComment       = \_ _   -> pure ()
                                , zlGetTicketComments       = \_     -> pure []
                                , zlGetAttachment           = \_     -> pure Nothing
                                }

                    let stubbedConfig :: Config
                        stubbedConfig = withStubbedIOAndDataLayer stubbedDataLayer

                    let appExecution :: IO ZendeskResponse
                        appExecution = runApp (processTicket . tiId $ ticketInfo) stubbedConfig

                    eZendeskResponse <- run (try appExecution)

                    -- Check it throws exception
                    assert $ isLeft (eZendeskResponse :: Either ProcessTicketExceptions ZendeskResponse)
                    whenLeft eZendeskResponse $ \processException ->
                        assert $ processException == CommentAndAttachmentNotFound (tiId ticketInfo)

        it "tries to process ticket but cannot find attachments, throws exception" $
            forAll arbitrary $ \(ticketInfo :: TicketInfo) ->
                forAll (listOf1 arbitrary) $ \comments ->

                    monadicIO $ do

                        pre $ any (\Comment{..} -> not $ null cAttachments) comments

                        let stubbedDataLayer :: DataLayer App
                            stubbedDataLayer =
                                emptyDataLayer
                                    { zlGetTicketInfo           = \_     -> pure $ Just ticketInfo
                                    , zlPostTicketComment       = \_ _   -> pure ()
                                    , zlGetTicketComments       = \_     -> pure comments
                                    , zlGetAttachment           = \_     -> pure Nothing
                                    }

                        let stubbedConfig :: Config
                            stubbedConfig = withStubbedIOAndDataLayer stubbedDataLayer

                        let appExecution :: IO ZendeskResponse
                            appExecution = runApp (processTicket . tiId $ ticketInfo) stubbedConfig

                        eZendeskResponse <- run (try appExecution)

                        -- Check we have some comments.
                        assert $ isLeft (eZendeskResponse :: Either ProcessTicketExceptions ZendeskResponse)
                        whenLeft eZendeskResponse $ \processException ->
                            assert $ processException == AttachmentNotFound (tiId ticketInfo)

        it "tries to process ticket but cannot fetch ticket info, throws exception" $
            forAll arbitrary $ \(ticketId :: TicketId) ->

                    monadicIO $ do

                        let stubbedDataLayer :: DataLayer App
                            stubbedDataLayer =
                                emptyDataLayer
                                    { zlGetTicketInfo           = \_     -> pure Nothing
                                    , zlPostTicketComment       = \_ _   -> pure ()
                                    , zlGetTicketComments       = \_     -> pure empty
                                    }

                        let stubbedConfig :: Config
                            stubbedConfig = withStubbedIOAndDataLayer stubbedDataLayer

                        let appExecution :: IO ZendeskResponse
                            appExecution = runApp (processTicket ticketId) stubbedConfig

                        eZendeskResponse <- run (try appExecution)

                        assert $ isLeft (eZendeskResponse :: Either ProcessTicketExceptions ZendeskResponse)
                        whenLeft eZendeskResponse $ \processException ->
                            assert $ processException == TicketInfoNotFound ticketId

isResponseTaggedWithNoLogs :: ZendeskResponse -> Bool
isResponseTaggedWithNoLogs response = "no-log-files" `elem` getTicketTags (zrTags response)

genCommentWithNoAttachment :: Gen Comment
genCommentWithNoAttachment = Comment
    <$> arbitrary
    <*> arbitrary
    <*> return mempty
    <*> arbitrary
    <*> arbitrary

genCommentWithAttachment :: Gen Comment
genCommentWithAttachment = Comment
    <$> arbitrary
    <*> arbitrary
    <*> listOf1 arbitrary
    <*> arbitrary
    <*> arbitrary

genTicketWithStatus :: Gen TicketInfo
genTicketWithStatus = TicketInfo
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (TicketStatus <$> elements ["open", "closed"])
    <*> arbitrary
    <*> arbitrary

genTicketWithFilteredTags :: [Text] -> Gen TicketInfo
genTicketWithFilteredTags tagToBeFiltered = TicketInfo
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> return (TicketTags tagToBeFiltered)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

genTicketWithUnsolvedStatus :: Gen TicketInfo
genTicketWithUnsolvedStatus = TicketInfo
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (TicketStatus <$> elements ["new", "hold", "open", "pending"])
    <*> arbitrary
    <*> arbitrary

processTicketsSpec :: Spec
processTicketsSpec =
    describe "processTickets" $ do
        it "doesn't process tickets" $ do
            pending

-- Simple tests to cover it works.
validShowURLSpec :: Spec
validShowURLSpec =
    describe "showURL" $ modifyMaxSuccess (const 10000) $ do
        it "returns valid UserRequestedTicketsURL" $ do
            property $ \userId ->
                let typedURL    = showURL $ UserRequestedTicketsURL userId
                    untypedURL  = "/users/" <> show (getUserId userId) <> "/tickets/requested.json"
                in  typedURL == untypedURL
        it "returns valid UserAssignedTicketsURL" $ do
            property $ \userId ->
                let typedURL    = showURL $ UserAssignedTicketsURL userId
                    untypedURL  = "/users/" <> show (getUserId userId) <> "/tickets/assigned.json"
                in  typedURL == untypedURL
        it "returns valid UserUnassignedTicketsURL" $
                let typedURL    = showURL $ UserUnassignedTicketsURL
                    untypedURL  = "/search.json?query=type%3Aticket%20assignee%3Anone&sort_by=created_at&sort_order=asc"
                in  typedURL == untypedURL
        it "returns valid TicketsURL" $ do
            property $ \ticketId ->
                let typedURL    = showURL $ TicketsURL ticketId
                    untypedURL  = "/tickets/" <> show (getTicketId ticketId) <> ".json"
                in  typedURL == untypedURL
        it "returns valid TicketAgentURL" $ do
            property $ \ticketId ->
                let typedURL    = showURL $ TicketAgentURL ticketId
                    untypedURL  = "https://iohk.zendesk.com/agent/tickets/" <> show (getTicketId ticketId)
                in  typedURL == untypedURL
        it "returns valid TicketCommentsURL" $ do
            property $ \ticketId ->
                let typedURL    = showURL $ TicketCommentsURL ticketId
                    untypedURL  = "/tickets/" <> show (getTicketId ticketId) <> "/comments.json"
                in  typedURL == untypedURL
        it "returns valid ExportDataByTimestamp" $ do
            property $ \timestamp ->
                let typedURL    = showURL $ ExportDataByTimestamp timestamp
                    untypedURL  = "https://iohk.zendesk.com/api/v2/incremental/tickets.json?start_time="
                        <> (show @_ @Integer . floor . toRational $ getExportFromTime timestamp)
                in  typedURL == untypedURL


filterTicketsWithAttachmentsSpec :: Spec
filterTicketsWithAttachmentsSpec =
    describe "filterTicketsWithAttachments" $ do
        it "filtered/unfiltered tickets return same length if all tickets have attachments" $ do
            forAll (listOf1 arbitrary) $ \(listTickets) ->
                forAll (listOf1 genCommentWithAttachment) $ \(comments) ->

                    monadicIO $ do

                        let stubbedDataLayer :: DataLayer App
                            stubbedDataLayer =
                                emptyDataLayer
                                    {
                                      zlGetTicketComments       = \_     -> pure comments
                                    }
                        let stubbedConfig :: Config
                            stubbedConfig = withStubbedIOAndDataLayer stubbedDataLayer

                        let appExecution :: IO [TicketInfo]
                            appExecution = runApp (filterTicketsWithAttachments listTickets) stubbedConfig

                        tickets <- run appExecution
                        -- Check we have some tickets.
                        assert $ (length tickets) == (length listTickets)

        it "filtered tickets length is zero if no tickets have attachments" $ do
            forAll (listOf1 arbitrary) $ \(listTickets) ->
                forAll (listOf1 genCommentWithNoAttachment) $ \(comments) ->

                    monadicIO $ do

                        let stubbedDataLayer :: DataLayer App
                            stubbedDataLayer =
                                emptyDataLayer
                                    { zlGetTicketComments       = \_     -> pure comments
                                    }
                        let stubbedConfig :: Config
                            stubbedConfig = withStubbedIOAndDataLayer stubbedDataLayer

                        let appExecution :: IO [TicketInfo]
                            appExecution = runApp (filterTicketsWithAttachments listTickets) stubbedConfig

                        tickets <- run appExecution
                        -- Check we have some tickets.
                        assert $ (length tickets) == 0

filterTicketsByStatusSpec :: Spec
filterTicketsByStatusSpec =
    describe "filterTicketsByStatus" $ do
        it "a TicketInfo array with x open tickets should filter to length x" $ property $
            forAll (listOf1 genTicketWithStatus) $ \tickets -> do
                length (filterTicketsByStatus tickets "open") == length (filter ((== TicketStatus "open") . tiStatus) tickets)
        it "a TicketInfo array with x closed tickets should filter to length x" $ property $
            forAll (listOf1 genTicketWithStatus) $ \tickets -> do
                length (filterTicketsByStatus tickets "closed") == length (filter ((== TicketStatus "closed") . tiStatus) tickets)

showAttachmentInfoSpec :: Spec
showAttachmentInfoSpec =
    describe "showAttachmentInfo" $ do
        it "given an attachment, return  a Text describing the attachment" $ property $
            forAll (listOf1 arbitrary) $ \(listOfAttachments :: [Attachment]) ->
                fmap showAttachmentInfo listOfAttachments == fmap (\attachment -> ("  Attachment: " <> (show $ aSize attachment) <> " - " <> aURL attachment :: Text)) listOfAttachments

showCommentAttachmentsSpec :: Spec
showCommentAttachmentsSpec =
    describe "showCommentsAttachments" $ do
        it "given an comment, return  a [Text] describing each comment` attachment" $ property $
            forAll (listOf1 arbitrary) $ \(listOfComments :: [Comment]) ->
                fmap showCommentAttachments listOfComments == fmap (\comment -> ( showAttachmentInfo <$> cAttachments comment)) listOfComments

showTicketCategoryCountSpec :: Spec
showTicketCategoryCountSpec =
    describe "showTicketCategoryCount" $ do
        it "Properly show  total/open/closed ticket count" $ property $
            forAll (listOf1 arbitrary) $ \(listOfComments :: [Comment]) ->
                fmap showCommentAttachments listOfComments == fmap (\comment -> ( showAttachmentInfo <$> cAttachments comment)) listOfComments

filterAnalyzedTicketsSpec :: Spec
filterAnalyzedTicketsSpec =
    describe "filterAnalyzedTickets" $ modifyMaxSuccess (const 200) $ do
        it "should not filter tickets with status 'open', 'hold', 'pending', and 'new'" $
            forAll (listOf genTicketWithUnsolvedStatus) $ \(ticketInfos :: [TicketInfo]) ->
                    length (filterAnalyzedTickets ticketInfos) `shouldBe` length ticketInfos

        it "should filter solved tickets" $
            forAll (listOf arbitrary) $ \(ticketInfos :: [TicketInfo]) ->
                let filteredTickets :: [TicketInfo]
                    filteredTickets = filterAnalyzedTickets ticketInfos
                in all (\ticket -> tiStatus ticket /= TicketStatus "solved") filteredTickets

        it "should filter goguen testnet tickets" $
            forAll (listOf $ genTicketWithFilteredTags ["goguen_testnets"]) $
                \(ticketInfos :: [TicketInfo]) ->
                    length (filterAnalyzedTickets ticketInfos) `shouldBe` 0

        it "should filter analyzed tickets" $
            forAll (listOf $ genTicketWithFilteredTags ["analyzed-by-script-v1.0"]) $
                \(ticketInfos :: [TicketInfo]) ->
                    length (filterAnalyzedTickets ticketInfos) `shouldBe` 0

createResponseTicketSpec :: Spec
createResponseTicketSpec =
    describe "createResponseTicket" $ modifyMaxSuccess (const 200) $ do
        it "should preserve ticket field and custom field from ticketInfo" $
            property $ \agentId ticketInfo zendeskResponse ->
                let responseTicket = createResponseTicket agentId ticketInfo zendeskResponse
                in tiField ticketInfo == tField responseTicket
                && tiCustomField ticketInfo == tCustomField responseTicket
        it "should preserve tags from ticketinfo and zendeskresponse" $
            property $ \agentId ticketInfo zendeskResponse ->
                let responseTicket      = createResponseTicket agentId ticketInfo zendeskResponse
                    mergedTags          = getTicketTags $ tiTags ticketInfo <> zrTags zendeskResponse
                    responseTags        = getTicketTags $ tTag responseTicket
                -- in summary, the response tags have the debuggers `analyzed-by-script-version` tag AND
                -- they remove the `to_be_analysed` tag AND they are unique.
                in (filter (/= renderTicketStatus ToBeAnalyzed) . nub $ renderTicketStatus AnalyzedByScriptV1_4_3 : mergedTags) === responseTags


exportZendeskDataToLocalDBSpec :: Spec
exportZendeskDataToLocalDBSpec =
    describe "exportZendeskDataToLocalDB" $ modifyMaxSuccess (const 200) $ do
        it "should export no data since there is nothing new" $
            forAll arbitrary $ \(exportFromTime) ->

               monadicIO $ do

                   let stubbedDataLayer :: DataLayer App
                       stubbedDataLayer =
                           emptyDataLayer
                               { zlExportTickets           = \_     -> pure []
                               , zlListDeletedTickets      =           pure []
                               , zlGetTicketComments       = \_     -> pure []
                               }

                   let stubbedConfig :: Config
                       stubbedConfig = withStubbedIOAndDataLayer stubbedDataLayer

                   let appExecution :: IO [TicketInfo]
                       appExecution = runApp (exportZendeskDataToLocalDB mapNotConcurrent exportFromTime) stubbedConfig

                   ticketsToExport <- run appExecution

                   -- Check we don't have any tickets.
                   assert . null $ ticketsToExport

        it "should export a list of tickets since there are new tickets" $
             forAll (listOf1 genTicketWithUnsolvedStatus) $ \(listTickets) ->
                forAll arbitrary $ \(exportFromTime) ->

                    monadicIO $ do

                        let stubbedDataLayer :: DataLayer App
                            stubbedDataLayer =
                                emptyDataLayer
                                    { zlExportTickets           = \_     -> pure listTickets
                                    , zlListDeletedTickets      =           pure []
                                    , zlGetTicketComments       = \_     -> pure []
                                    }

                        let stubbedConfig :: Config
                            stubbedConfig = withStubbedIOAndDataLayer stubbedDataLayer

                        let appExecution :: IO [TicketInfo]
                            appExecution = runApp (exportZendeskDataToLocalDB mapNotConcurrent exportFromTime) stubbedConfig

                        ticketsToExport <- run appExecution

                        -- Check that we have tickets.
                        assert . not . null $ ticketsToExport
                        assert $ length ticketsToExport == length listTickets

        it "should not return deleted tickets" $
             forAll (listOf1 arbitrary) $ \(listTickets) ->
                forAll arbitrary $ \(exportFromTime) ->

                    monadicIO $ do

                        let stubbedDataLayer :: DataLayer App
                            stubbedDataLayer =
                                emptyDataLayer
                                    { zlExportTickets           = \_     -> pure listTickets
                                    , zlListDeletedTickets      =           pure $ map (DeletedTicket . tiId) listTickets
                                    , zlGetTicketComments       = \_     -> pure []
                                    }

                        let stubbedConfig :: Config
                            stubbedConfig = withStubbedIOAndDataLayer stubbedDataLayer

                        let appExecution :: IO [TicketInfo]
                            appExecution = runApp (exportZendeskDataToLocalDB mapNotConcurrent exportFromTime) stubbedConfig

                        ticketsToExport <- run appExecution

                        -- Check we don't have any tickets.
                        assert . null $ ticketsToExport

        it "should not return duplicated tickets" $
             forAll (listOf1 genTicketWithUnsolvedStatus) $ \(listTickets) ->
                forAll arbitrary $ \(exportFromTime) ->

                    monadicIO $ do

                        -- Here we duplicate them to check if the function works.
                        let duplicatedListTickets = concat $ replicate 10 listTickets

                        let stubbedDataLayer :: DataLayer App
                            stubbedDataLayer =
                                emptyDataLayer
                                    { zlExportTickets           = \_     -> pure duplicatedListTickets
                                    , zlListDeletedTickets      =           pure []
                                    , zlGetTicketComments       = \_     -> pure []
                                    }

                        let stubbedConfig :: Config
                            stubbedConfig = withStubbedIOAndDataLayer stubbedDataLayer

                        let appExecution :: IO [TicketInfo]
                            appExecution = runApp (exportZendeskDataToLocalDB mapNotConcurrent exportFromTime) stubbedConfig

                        ticketsToExport <- run appExecution

                        -- Check that we have tickets.
                        assert . not . null $ ticketsToExport
                        assert $ length ticketsToExport == length listTickets
  where
    -- [a] -> Int -> Int -> (a -> m b) -> m [b]
    mapNotConcurrent
        :: [TicketInfo]
        -> Int
        -> Int
        -> (TicketInfo -> App (TicketInfo, [Comment]))
        -> App [(TicketInfo, [Comment])]
    mapNotConcurrent tickets _ _ fetchTicketData = forM tickets fetchTicketData

getAttachmentsFromCommentSpec :: Spec
getAttachmentsFromCommentSpec =
    describe "getAttachmentsFromComment" $ modifyMaxSuccess (const 200) $
        prop "should return zip file attachments" $
          \(comments :: [Comment]) -> do
            let filteredAttachments = getAttachmentsFromComment comments
            all (\att -> aContentType att `elem` zipFileContentTypes) filteredAttachments
          where
            zipFileContentTypes :: [Text]
            zipFileContentTypes = ["application/zip", "application/x-zip-compressed"]

------------------------------------------------------------
-- Tests for database operations
------------------------------------------------------------

-- Currently, this is how datas are stored in the DB, this can be changed in the future.
type DBTicketInfo = (TicketId, UserId, Maybe UserId, TicketURL, TicketTags, TicketStatus)
type DBTicketComment = (CommentId, TicketId, CommentBody, Bool, Integer)
type DBCommentAttachment = (AttachmentId, CommentId, Text, Text, Int)
type DBAttachmentContent = (Integer, ByteString)

deleteAllDataSpec :: Spec
deleteAllDataSpec =
    describe "deleteAllData" $ modifyMaxSuccess (const 200) $ do
       prop "should delete all datas from database" $
           monadicIO $ do
               (tickets, ticketComments, commentAttachments, attachmentContents) <- 
                   run $ withDBSchema ":memory:" (\conn -> do
                       deleteAllData conn
                       tickets            <- query_ conn "SELECT * from ticket_info"
                       ticketComments     <- query_ conn "SELECT * from ticket_comment"
                       commentAttachments <- query_ conn "SELECT * from comment_attachment"
                       attachmentContents <- query_ conn "SELECT * from attachment_content"
                       return (tickets, ticketComments, commentAttachments, attachmentContents)
                       )

               assert . null $ (tickets            :: [DBTicketInfo])
               assert . null $ (ticketComments     :: [DBTicketComment])
               assert . null $ (commentAttachments :: [DBCommentAttachment])
               assert . null $ (attachmentContents :: [DBAttachmentContent])

       prop "should try to delete datas from database without tables, throws exception" $
           monadicIO $ do
               eResult <- run . try $ withConnection ":memory:" deleteAllData

               assert $ isLeft (eResult :: Either DBLayerException ())

insertCommentAttachmentsSpec :: Spec
insertCommentAttachmentsSpec =
    describe "insertCommentAttachmentsSpec" $ modifyMaxSuccess (const 200) $ do
        prop "should insert comment and its attachment to the database" $
            \(comment :: Comment) (attachment :: Attachment) ->
                monadicIO $ do
                    attachments <- run $
                        withDBSchema ":memory:" (\conn -> do
                            insertCommentAttachments conn comment attachment
                            attachments <- queryNamed conn
                                "SELECT * FROM comment_attachment WHERE aId = :id"
                                [":id" := aId attachment]
                            return (attachments :: [DBCommentAttachment])
                            )

                    assert . isJust . safeHead $ attachments
                    whenJust (safeHead attachments)
                       (\(attachmentId, commentId, attUrl, attContentType, attSize) -> do
                           assert $ attachmentId   == aId attachment
                           assert $ commentId      == cId comment
                           assert $ attUrl         == aURL attachment
                           assert $ attContentType == aContentType attachment
                           assert $ attSize        == aSize attachment
                        )

        prop "should try to insert comment and its attachment to the database without tables, throws exception" $
            \(comment :: Comment) (attachment :: Attachment) ->
                monadicIO $ do
                    eResult <- run . try $
                        withConnection ":memory:"
                        (\conn -> insertCommentAttachments conn comment attachment)

                    assert $ isLeft (eResult :: Either DBLayerException ())

insertTicketInfoSpec :: Spec
insertTicketInfoSpec =
    describe "insertTicketInfo" $ modifyMaxSuccess (const 200) $ do
      prop "should insert ticket info to the dabatase" $
          \(ticketInfo :: TicketInfo) ->
              monadicIO $ do
                ticketInfos <- run $
                    withDBSchema ":memory:" (\conn -> do
                        insertTicketInfo conn ticketInfo
                        ticketInfos <- queryNamed conn "SELECT * FROM ticket_info WHERE tiId = :id"
                            [":id" := (getTicketId . tiId) ticketInfo]
                        return (ticketInfos :: [DBTicketInfo])
                        )

                assert . isJust . safeHead $ ticketInfos
                whenJust (safeHead ticketInfos)
                    (\(ticketId, requesterId, assigneeId, ticketUrl, ticketTags, ticketStatus) -> do
                       assert $ ticketId     == tiId ticketInfo
                       assert $ requesterId  == tiRequesterId ticketInfo
                       assert $ assigneeId   == tiAssigneeId ticketInfo
                       assert $ ticketUrl    == tiUrl ticketInfo
                       assert $ ticketTags   == tiTags ticketInfo
                       assert $ ticketStatus == tiStatus ticketInfo
                    )
      prop "should try to insert ticketInfo to the database without tables, throws exception" $
          \(ticketInfo :: TicketInfo) ->
              monadicIO $ do
                eResult <- run . try $
                    withConnection ":memory:" (\conn -> insertTicketInfo conn ticketInfo)

                assert $ isLeft (eResult :: Either DBLayerException ())

insertTicketCommentsSpec :: Spec
insertTicketCommentsSpec =
    describe "insertTicketComments" $ modifyMaxSuccess (const 200) $ do
      prop "should insert ticketId and its comments to the dabatase" $
          \(ticketId :: TicketId) (comment :: Comment) ->
              monadicIO $ do
               comments <- run $
                    withDBSchema ":memory:" (\conn -> do
                        insertTicketComments conn ticketId comment
                        comments <- queryNamed conn
                            "SELECT * FROM ticket_comment WHERE ticket_id = :id"
                            [":id" := getTicketId ticketId]
                        return (comments :: [DBTicketComment])
                    )

               assert . isJust . safeHead $ comments
               whenJust (safeHead comments) (\(commentId, tid, commentbody, isPublic, authorId) -> do
                   assert $ tid         == ticketId
                   assert $ commentbody == cBody comment
                   assert $ commentId   == cId comment
                   assert $ isPublic    == cPublic comment
                   assert $ authorId    == cAuthor comment
                )
      prop "should try to insert ticketId and comment to the database without tables, throws exception" $
          \(ticketId :: TicketId) (comment :: Comment) ->
              monadicIO $ do
                eResult <- run . try $
                    withConnection ":memory:" (\conn -> insertTicketComments conn ticketId comment)

                assert $ isLeft (eResult :: Either DBLayerException ())

-- | Perform an action to the database with schema
withDBSchema :: String -> (Connection -> IO a) -> IO a
withDBSchema dbpath action =
    withConnection dbpath (\conn -> do
        createSchema conn
        action conn
        )
