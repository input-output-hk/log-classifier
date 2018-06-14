module Main where

import           Universum

import           Test.Hspec (Spec, describe, hspec, it, pending)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (Gen, arbitrary, forAll, listOf1)
import           Test.QuickCheck.Monadic (assert, monadicIO, pre, run)

import           Lib (processTicket, listAndSortTickets)
import           Zendesk (App, Comment (..), Config (..), IOLayer (..), TicketInfo (..),
                          ZendeskLayer (..), ZendeskResponse (..), basicIOLayer, defaultConfig,
                          emptyZendeskLayer, runApp)

-- TODO(ks): What we are really missing is a realistic @Gen ZendeskLayer m@.

main :: IO ()
main = hspec spec

-- stack test log-classifier --fast --test-arguments "-m Zendesk"
spec :: Spec
spec =
    describe "Zendesk" $ do
        listAndSortTicketsSpec
        processTicketSpec


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
                                { zlListTickets     = \_     -> pure []
                                , zlGetTicketInfo   = \_     -> pure ticketInfo
                                }

                    let stubbedConfig :: Config
                        stubbedConfig = withStubbedIOAndZendeskLayer stubbedZendeskLayer

                    let appExecution :: IO [TicketInfo]
                        appExecution = runApp listAndSortTickets stubbedConfig

                    tickets <- run appExecution

                    assert $ length tickets == 0

        it "returns sorted nonempty tickets" $ do
            forAll arbitrary $ \(ticketInfo) ->
                forAll (listOf1 arbitrary) $ \(listTickets) -> do

                    monadicIO $ do

                        let stubbedZendeskLayer :: ZendeskLayer App
                            stubbedZendeskLayer =
                                emptyZendeskLayer
                                    { zlListTickets     = \_     -> pure listTickets
                                    , zlGetTicketInfo   = \_     -> pure ticketInfo
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
                                    { zlListTickets         = \_     -> pure listTickets
                                    , zlGetTicketInfo       = \_     -> pure ticketInfo
                                    , zlPostTicketComment   = \_     -> pure ()
                                    , zlGetTicketComments   = \_     -> pure []
                                    }

                        let stubbedConfig :: Config
                            stubbedConfig = withStubbedIOAndZendeskLayer stubbedZendeskLayer

                        let appExecution :: IO (Maybe ZendeskResponse)
                            appExecution = runApp (processTicket . ticketId $ ticketInfo) stubbedConfig

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
                                    { zlListTickets         = \_     -> pure listTickets
                                    , zlGetTicketInfo       = \_     -> pure ticketInfo
                                    , zlPostTicketComment   = \_     -> pure ()
                                    , zlGetTicketComments   = \_     -> pure comments
                                    , zlGetAttachment       = \_     -> pure mempty
                                    }

                        let stubbedConfig :: Config
                            stubbedConfig = withStubbedIOAndZendeskLayer stubbedZendeskLayer

                        let appExecution :: IO (Maybe ZendeskResponse)
                            appExecution = runApp (processTicket . ticketId $ ticketInfo) stubbedConfig

                        zendeskResponse <- run appExecution

                        -- Check we have some comments.
                        assert $ isJust zendeskResponse

        it "processes ticket, with no attachments" $
            forAll (listOf1 genCommentWithNoAttachment) $ \(commentsWithoutAttachment :: [Comment]) ->
                forAll arbitrary $ \ticketInfo ->
                    monadicIO $ do

                    let stubbedZendeskLayer :: ZendeskLayer App
                        stubbedZendeskLayer =
                            emptyZendeskLayer
                                { zlGetTicketComments = \_ -> pure commentsWithoutAttachment
                                , zlGetTicketInfo     = \_ -> pure ticketInfo
                                , zlPostTicketComment = \_ -> pure ()
                                }

                    let stubbedConfig :: Config
                        stubbedConfig = withStubbedIOAndZendeskLayer stubbedZendeskLayer

                    let appExecution :: IO (Maybe ZendeskResponse)
                        appExecution = runApp (processTicket . ticketId $ ticketInfo) stubbedConfig

                    zendeskResponse <- run appExecution

                    assert $ isJust zendeskResponse
                    assert $ isResponseTaggedWithNoLogs zendeskResponse

        it "processes ticket, with attachments" $
            forAll (listOf1 arbitrary) $ \(comments:: [Comment]) ->
                forAll arbitrary $ \ticketInfo ->
                    monadicIO $ do

                    pre $ any (not . null . cAttachments) comments

                    let stubbedZendeskLayer :: ZendeskLayer App
                        stubbedZendeskLayer =
                            emptyZendeskLayer
                                { zlGetTicketComments = \_ -> pure comments
                                , zlGetTicketInfo     = \_ -> pure ticketInfo
                                , zlPostTicketComment = \_ -> pure ()
                                , zlGetAttachment     = \_ -> pure mempty
                                }

                    let stubbedConfig :: Config
                        stubbedConfig = withStubbedIOAndZendeskLayer stubbedZendeskLayer

                    let appExecution :: IO (Maybe ZendeskResponse)
                        appExecution = runApp (processTicket . ticketId $ ticketInfo) stubbedConfig

                    zendeskResponse <- run appExecution
                    
                    assert $ isJust zendeskResponse
                    assert $ not (isResponseTaggedWithNoLogs zendeskResponse)

isResponseTaggedWithNoLogs :: Maybe ZendeskResponse -> Bool
isResponseTaggedWithNoLogs (Just response) = "no-log-files" `elem` zrTags response
isResponseTaggedWithNoLogs Nothing         = False

genCommentWithNoAttachment :: Gen Comment
genCommentWithNoAttachment = Comment
    <$> arbitrary
    <*> return mempty
    <*> arbitrary
    <*> arbitrary

processTicketsSpec :: Spec
processTicketsSpec =
    describe "processTickets" $ do
        it "doesn't process tickets" $ do
            pending
