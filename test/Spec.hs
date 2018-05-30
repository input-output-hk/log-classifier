module Main where

import           Universum

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Zendesk
import           Lib

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
        it "processes ticket, no comments" $ do
            forAll arbitrary $ \(ticketInfo :: TicketInfo) ->
                forAll (listOf1 arbitrary) $ \(listTickets) -> do

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

                        let appExecution :: IO [ZendeskResponse]
                            appExecution = runApp (processTicket . ticketId $ ticketInfo) stubbedConfig

                        zendeskComments <- run appExecution

                        -- Check we have some comments.
                        assert $ length zendeskComments == 0

        it "processes ticket, with comments" $ do
            forAll arbitrary $ \(ticketInfo :: TicketInfo) ->
                forAll (listOf1 arbitrary) $ \(listTickets) -> do
                forAll (listOf1 arbitrary) $ \(comments) -> do


                    monadicIO $ do

                        -- A simple precondition.
                        pre $ any (\comment -> length (cAttachments comment) > 0) comments

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

                        let appExecution :: IO [ZendeskResponse]
                            appExecution = runApp (processTicket . ticketId $ ticketInfo) stubbedConfig

                        zendeskResponses <- run appExecution

                        -- Check we have some comments.
                        assert $ length zendeskResponses > 0

processTicketsSpec :: Spec
processTicketsSpec =
    describe "processTickets" $ do
        it "doesn't process tickets" $ do
            pending
