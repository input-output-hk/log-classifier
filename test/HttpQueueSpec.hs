module HttpQueueSpec
    ( dispatchActionsSpec
    ) where

import           Universum

import           Control.Concurrent.Classy (MonadConc, fork)

import           UnliftIO.Async (mapConcurrently)

import           Network.HTTP.Simple (defaultRequest)

import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (Arbitrary (..), Gen, Property, choose, oneof, property)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import           Test.DejaFu (deadlocksNever, exceptionsNever)
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit.DejaFu (testDejafu)

import           Http.Exceptions (HttpNetworkLayerException (..))
import           Http.Queue (createSchedulerConfig, createSchedulerConfigTime, runDispatch,
                             runScheduler)


-- stack test log-classifier --fast --test-arguments "-m dispatchActionsSpec"
dispatchActionsSpec :: Spec
dispatchActionsSpec = do
    -- The tests might take longer, but we don't want to have a failure here.
    describe "dispatchActionsSpec" $ modifyMaxSuccess (const 1000) $ do

        -- Testing from the DejaFu run.
        -- fromHUnitTest $ testDejafu "Assert it never abort" abortsNever simpleRun -- kills memory
        fromHUnitTest $ testDejafu "Assert it never deadlocks" deadlocksNever simpleRun
        fromHUnitTest $ testDejafu "Assert it doesn't raise exceptions" exceptionsNever simpleRun

        it "should dispatch all the actions and return their results" $
            property generateShedulerSpec

        it "should dispatch all the actions and return the exception if they raise an exception" $
            property generateShedulerExceptionalSpec
  where
    -- | A simple test for consistency.
    simpleRun :: forall m. (MonadConc m) => m ()
    simpleRun = do

        -- Create the configuration
        schedulerConfig <- createSchedulerConfig 3

        -- Run the sheduler (thread).
        _               <- runScheduler schedulerConfig

        -- Run the dispatch actions in parallel. Use 3 times more requests so it
        -- does take queuing into consideration.
        void . replicateM 10 . fork $ runDispatch schedulerConfig (pure ())

newtype ShedulerExample = ShedulerExample (Int, Int, Int)
    deriving (Show, Eq)

instance Arbitrary ShedulerExample where
    arbitrary = generateShedulerExample

-- | The result type.
data DispatchResultType a
    = ValueResult a
    | Exception HttpNetworkLayerException
    deriving (Show)


instance (Arbitrary a) => Arbitrary (DispatchResultType a) where
    arbitrary = oneof
        [ ValueResult <$> arbitrary
        , Exception <$> arbitrary
        ]

-- | A simple uniform generator.
generateShedulerExample :: Gen ShedulerExample
generateShedulerExample = do
    dispatches  <- choose (0, 1000)
    rateLimit   <- choose (1, 1000)
    delay       <- choose (1, 10000)

    pure $ ShedulerExample (dispatches, rateLimit, delay)

-- | The spec for the dispatch queue.
generateShedulerSpec
    :: ShedulerExample
    -> Property
generateShedulerSpec (ShedulerExample (dispatches, rateLimit, delay)) =
    monadicIO $ do

        -- Create the configuration
        schedulerConfig <- run $ createSchedulerConfigTime rateLimit delay
        -- Run the sheduler (thread).
        _               <- run $ runScheduler schedulerConfig
        -- Run the dispatch actions in parallel.
        results         <- run $
            mapConcurrently
                (runDispatch schedulerConfig . pure)
                [1..dispatches]

        -- We better not miss something!
        assert $ length results == dispatches

-- | The spec for the exceptional dispatch queue.
generateShedulerExceptionalSpec
    :: ShedulerExample
    -> Property
generateShedulerExceptionalSpec (ShedulerExample (dispatches, rateLimit, delay)) =
    monadicIO $ do

        -- Create the configuration
        schedulerConfig <- run $ createSchedulerConfigTime rateLimit delay
        -- Run the sheduler (thread).
        _               <- run $ runScheduler schedulerConfig
        -- Run the dispatch actions in parallel.
        results         <- run $
            mapConcurrently
                (try @_ @HttpNetworkLayerException <$>
                    runDispatch schedulerConfig . pure (throwM $ HttpUnauthorized defaultRequest))
                [1..dispatches]

        -- We better not miss something!
        assert $ length results == dispatches
        -- All of them should fail!
        assert $ (length . filter isLeft $ results) == dispatches

