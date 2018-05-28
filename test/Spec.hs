module Main where

import           Universum

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Zendesk
import           Lib

main :: IO ()
main = hspec spec

-- stack test log-classifier --fast --test-arguments "-m Zendesk"
spec :: Spec
spec =
    describe "Zendesk" $ do
        processTicketSpec

-- TODO(ks): withStubbedZendeskLayer :: ZendeskLayer App -> Config


processTicketSpec :: Spec
processTicketSpec = do
    describe "processTicket" $ modifyMaxSuccess (const 10000) $ do
        it "doesn't process ticket since the ticket cannot be found" $ do
            forAll arbitrary $ \(ticketInfo :: TicketInfo) -> do

                monadicIO $ do

                    let stubbedZendeskLayer :: ZendeskLayer App
                        stubbedZendeskLayer =
                            basicZendeskLayer
                                { zlListTickets     = \_     -> pure []
                                , zlGetTicketInfo   = \_     -> pure ticketInfo
                                }

                    let stubbedConfig :: Config
                        stubbedConfig =
                            defaultConfig
                                { cfgZendeskLayer = stubbedZendeskLayer
                                }

                    let appExecution :: IO [TicketInfo]
                        appExecution = runApp (processBatchTickets "test@iohk.io") stubbedConfig

                    tickets <- run appExecution
                    assert $ length tickets == 0

processTicketsSpec :: Spec
processTicketsSpec =
    describe "processTickets" $ do
        it "doesn't process tickets" $ do
            pending
