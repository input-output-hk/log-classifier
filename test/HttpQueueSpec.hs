module HttpQueueSpec
    ( dispatchActionsSpec
    ) where

import           Universum

import           UnliftIO.Async (mapConcurrently)

import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (Gen, choose, forAll)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import           HttpQueue (createShedulerConfigTime, runDispatch, runSheduler)

-- Ideally, we would like to have a quickcheck-state-machine tests here.

-- stack test log-classifier --fast --test-arguments "-m dispatchActionsSpec"
dispatchActionsSpec :: Spec
dispatchActionsSpec =
    describe "dispatchActionsSpec" $ modifyMaxSuccess (const 2000) $ do
        it "should dispatch all the actions and return their results" $
            forAll generateShedulerExample $ \(dispatches, rateLimit, delay) ->
                monadicIO $ do

                    shedulerConfig  <- run $ createShedulerConfigTime rateLimit 10
                    _               <- run $ runSheduler shedulerConfig
                    results         <- run $ mapConcurrently (runDispatch shedulerConfig . pure . show) [1..dispatches]

                    assert $ length results == dispatches

-- | A simple uniform generator.
generateShedulerExample :: Gen (Int, Int, Int)
generateShedulerExample = do
    dispatches  <- choose (0, 1000)
    rateLimit   <- choose (1, 1000)
    delay       <- choose (1, 10000)

    pure (dispatches, rateLimit, delay)

