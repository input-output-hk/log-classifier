module Main where

import           Universum

import           Test.Hspec (Spec, describe, hspec, it, pending, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (Gen, arbitrary, forAll, listOf, listOf1, property, elements)
import           Test.QuickCheck.Monadic (assert, monadicIO, pre, run)

import           DataSource (App, Comment (..), Config (..), IOLayer (..), TicketId (..),
                             TicketInfo (..), TicketStatus (..), TicketTags (..), User, UserId (..),
                             ZendeskAPIUrl (..), ZendeskLayer (..), ZendeskResponse (..),
                             basicIOLayer, defaultConfig, emptyZendeskLayer, runApp, showURL)
import           Lib (filterAnalyzedTickets, listAndSortTickets, processTicket)

-- TODO(ks): What we are really missing is a realistic @Gen ZendeskLayer m@.

main :: IO ()
main = hspec spec

-- stack test log-classifier --fast --test-arguments "-m Zendesk"
spec :: Spec
spec =
    describe "Zendesk" $ do
        validShowURLSpec
        listAndSortTicketsSpec
        processTicketSpec
        filterAnalyzedTicketsSpec


-- | A utility function for testing which stubs IO and returns
-- the @Config@ with the @ZendeskLayer@ that was passed into it.
withStubbedIOAndZendeskLayer :: ZendeskLayer App -> Config
withStubbedIOAndZendeskLayer stubbedZendeskLayer =
    defaultConfig
        { cfgZendeskLayer   = stubbedZendeskLayer
        , cfgIOLayer        = stubbedIOLayer
        }
  where
    stubbedIOLayer :: IOLayer App
    stubbedIOLayer =
        basicIOLayer
            { iolPrintText      = \_     -> pure ()
            -- ^ Do nothing with the output
            }

listAndSortTicketsSpec :: Spec
listAndSortTicketsSpec =
    describe "listAndSortTickets" $ modifyMaxSuccess (const 200) $ do
        it "doesn't return tickets since there are none" $ do
            forAll arbitrary $ \(ticketInfo :: TicketInfo) -> do

                monadicIO $ do

                    let stubbedZendeskLayer :: ZendeskLayer App
                        stubbedZendeskLayer =
                            emptyZendeskLayer
                                { zlListAssignedTickets     = \_     -> pure []
                                , zlGetTicketInfo           = \_     -> pure $ Just ticketInfo
                                , zlListAdminAgents         =           pure []
                                }

                    let stubbedConfig :: Config
                        stubbedConfig = withStubbedIOAndZendeskLayer stubbedZendeskLayer

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

                            let stubbedZendeskLayer :: ZendeskLayer App
                                stubbedZendeskLayer =
                                    emptyZendeskLayer
                                        { zlListAssignedTickets     = \_     -> pure listTickets
                                        , zlGetTicketInfo           = \_     -> pure ticketInfo
                                        , zlListAdminAgents         =           pure agents
                                        }

                            let stubbedConfig :: Config
                                stubbedConfig = withStubbedIOAndZendeskLayer stubbedZendeskLayer

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
        it "processes ticket, no comments" $
            forAll arbitrary $ \(ticketInfo :: TicketInfo) ->
                forAll (listOf1 arbitrary) $ \(listTickets) ->

                    monadicIO $ do

                        let stubbedZendeskLayer :: ZendeskLayer App
                            stubbedZendeskLayer =
                                emptyZendeskLayer
                                    { zlListAssignedTickets     = \_     -> pure listTickets
                                    , zlGetTicketInfo           = \_     -> pure $ Just ticketInfo
                                    , zlPostTicketComment       = \_     -> pure ()
                                    , zlGetTicketComments       = \_     -> pure []
                                    }

                        let stubbedConfig :: Config
                            stubbedConfig = withStubbedIOAndZendeskLayer stubbedZendeskLayer

                        let appExecution :: IO (Maybe ZendeskResponse)
                            appExecution = runApp (processTicket . tiId $ ticketInfo) stubbedConfig

                        zendeskComments <- run appExecution

                        -- Check we have some comments.
                        assert $ isNothing zendeskComments

        it "processes ticket, with comments" $
            forAll arbitrary $ \(ticketInfo :: TicketInfo) ->
                forAll (listOf1 arbitrary) $ \(listTickets) ->
                forAll (listOf1 arbitrary) $ \(comments) ->

                    monadicIO $ do

                        let stubbedZendeskLayer :: ZendeskLayer App
                            stubbedZendeskLayer =
                                emptyZendeskLayer
                                    { zlListAssignedTickets     = \_     -> pure listTickets
                                    , zlGetTicketInfo           = \_     -> pure $ Just ticketInfo
                                    , zlPostTicketComment       = \_     -> pure ()
                                    , zlGetTicketComments       = \_     -> pure comments
                                    , zlGetAttachment           = \_     -> pure $ Just mempty
                                    }

                        let stubbedConfig :: Config
                            stubbedConfig = withStubbedIOAndZendeskLayer stubbedZendeskLayer

                        let appExecution :: IO (Maybe ZendeskResponse)
                            appExecution = runApp (processTicket . tiId $ ticketInfo) stubbedConfig

                        zendeskResponse <- run appExecution

                        -- Check we have some comments.
                        assert $ isJust zendeskResponse

        it "processes ticket, with no attachments" $
            forAll (listOf1 genCommentWithNoAttachment) $ \(commentsWithoutAttachment :: [Comment]) ->
                forAll arbitrary $ \(ticketInfo :: TicketInfo) ->
                    monadicIO $ do

                    let stubbedZendeskLayer :: ZendeskLayer App
                        stubbedZendeskLayer =
                            emptyZendeskLayer
                                { zlGetTicketComments = \_ -> pure commentsWithoutAttachment
                                , zlGetTicketInfo     = \_ -> pure $ Just ticketInfo
                                , zlPostTicketComment = \_ -> pure ()
                                }

                    let stubbedConfig :: Config
                        stubbedConfig = withStubbedIOAndZendeskLayer stubbedZendeskLayer

                    let appExecution :: IO (Maybe ZendeskResponse)
                        appExecution = runApp (processTicket . tiId $ ticketInfo) stubbedConfig

                    zendeskResponse <- run appExecution

                    assert $ isJust zendeskResponse
                    assert $ isResponseTaggedWithNoLogs zendeskResponse

        it "processes ticket, with attachments" $
            forAll (listOf1 arbitrary) $ \(comments :: [Comment]) ->
                forAll arbitrary $ \(ticketInfo :: TicketInfo) ->
                    monadicIO $ do

                    pre $ any (not . null . cAttachments) comments

                    let stubbedZendeskLayer :: ZendeskLayer App
                        stubbedZendeskLayer =
                            emptyZendeskLayer
                                { zlGetTicketComments = \_ -> pure comments
                                , zlGetTicketInfo     = \_ -> pure $ Just ticketInfo
                                , zlPostTicketComment = \_ -> pure ()
                                , zlGetAttachment     = \_ -> pure $ Just mempty
                                }

                    let stubbedConfig :: Config
                        stubbedConfig = withStubbedIOAndZendeskLayer stubbedZendeskLayer

                    let appExecution :: IO (Maybe ZendeskResponse)
                        appExecution = runApp (processTicket . tiId $ ticketInfo) stubbedConfig

                    zendeskResponse <- run appExecution

                    assert $ isJust zendeskResponse
                    assert $ not (isResponseTaggedWithNoLogs zendeskResponse)

isResponseTaggedWithNoLogs :: Maybe ZendeskResponse -> Bool
isResponseTaggedWithNoLogs (Just response) = "no-log-files" `elem` zrTags response
isResponseTaggedWithNoLogs Nothing         = False

genCommentWithNoAttachment :: Gen Comment
genCommentWithNoAttachment = Comment
    <$> arbitrary
    <*> arbitrary
    <*> return mempty
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

genTicketWithFilteredTags :: [Text] -> Gen TicketInfo
genTicketWithFilteredTags tagToBeFiltered = TicketInfo
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> return (TicketTags tagToBeFiltered)
    <*> arbitrary

genTicketWithUnsolvedStatus :: Gen TicketInfo
genTicketWithUnsolvedStatus = TicketInfo
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (TicketStatus <$> elements ["new", "hold", "open", "pending"])
