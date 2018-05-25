module Main where

import           Universum

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

main :: IO ()
main = hspec spec

-- stack test log-classifier --fast --test-arguments "-m Zendesk"
spec :: Spec
spec = do
    describe "Zendesk" $ do
        processTicketSpec
        processTicketsSpec

processTicketSpec :: Spec
processTicketSpec = do
    describe "processTicket" $ modifyMaxSuccess (const 200) $ do
        it "doesn't process ticket since the ticket cannot be found" $
            pendingWith "Not implemented!"

processTicketsSpec :: Spec
processTicketsSpec =
    describe "processTickets" $ do
        it "doesn't process tickets" $ do
            pending
