module Http.Queue
    ( SchedulerConfig
    , createSchedulerConfig
    , createSchedulerConfigTime
    , runScheduler
    , runDispatch
    ) where

import           Universum hiding (MVar, catch, newMVar)

import           Control.Concurrent.Classy (MVar, MonadConc, QSem, catch, fork, modifyMVar_,
                                            newMVar, newQSem, signalQSem, threadDelay, waitQSem)

import           Http.Exceptions (HttpNetworkLayerException (..))

------------------------------------------------------------
-- Rate limiting queue
------------------------------------------------------------

-- In the comments below we consider the delay to be 60 seconds and the maximum
-- dispatch actions should be 750, which is the case in the Zendesk API where we
-- use it.
--
-- The idea behind the current implementation is that we keep track
-- of the number of requests we dispatch to the HTTP server and if we ever
-- reach the limit (currently 750 in this scenario), we will block all
-- threads until a minute has passed. Why? Because after a minute, the rate limit is reset.
-- To keep track of time, we have another thread ("sheduler thread") that has a request counter
-- and that can keep track of the passed time, and every minute resets the number of
-- available @Request@ back to the original number (750 in this scenario).
-- That allows for all waiting blocks to awake and execute in FIFO order.

type NumberOfRequests = Int
type Microseconds = Int


-- | Configuration of the sheduler.
-- We don't want to export this.
data SchedulerConfig m =
    SchedulerConfig !(MVar m NumberOfRequests) !(QSem m) !(m ())

-- | We initialize the sheduler. We keep track of two variables so we can
-- definitivly say how many @Request@ do we need to give back to the queue.
createSchedulerConfig
    :: forall m. (MonadConc m)
    => NumberOfRequests
    -> m (SchedulerConfig m)
createSchedulerConfig numberOfRequests =
    createSchedulerConfigTime numberOfRequests (60 * 1000000)

-- | We initialize the sheduler with a time delay variable.
-- We keep track of two variables so we can definitivly say how many
-- @Request@ do we need to give back to the queue.
createSchedulerConfigTime
    :: forall m. (MonadConc m)
    => NumberOfRequests
    -> Microseconds
    -> m (SchedulerConfig m)
createSchedulerConfigTime numberOfRequests time = do
    when (numberOfRequests <= 0) $
       error "The number of requests should be higher then 1!"

    when (time <= 0) $
       error "The time delay should be higher then 1! Do note, this is microseconds."

    qSem        <- newQSem numberOfRequests
    initMVar    <- newMVar 0

    pure $ SchedulerConfig initMVar qSem (threadDelay time)


-- | Sheduler thread waits a minute and then restores the number of available slots
-- for @Request@.
runScheduler
    :: forall m. (MonadConc m)
    => (SchedulerConfig m)
    -> m ()
runScheduler (SchedulerConfig numberOfRequests qsem delay) =
    void . fork $ forever $ do
        -- we wait for a minute, the rate-limiting resets every minute
        _ <- delay

        -- thread safe access
        modifyMVar_ numberOfRequests $ \num -> do
            -- we add the signals back to the queue
            replicateM_ num (signalQSem qsem)
            -- we need to reset the counter to zero
            pure 0


-- | What we need to use for the dispatch of the actions.
runDispatch
    :: forall m a. (MonadConc m, MonadMask m)
    => (SchedulerConfig m)
    -> m a
    -> m a
runDispatch sc@(SchedulerConfig numberOfRequests qsem delay) action = do
    -- block until we find a slot to send the request
    _ <- waitQSem qsem

    -- Currently it does take into account if rate limit kicks in!
    -- We need to add the number of requests, if it fails or if it succeeds, both cases.
    caughtAction `finally` (modifyMVar_ numberOfRequests $ \num -> pure $ succ num)
  where
    -- | If we ever reach the @HttpTooManyRequests@ exception, which is a signal for the
    -- rate limit kicking in, we can run the action dispatch after a minute.
    -- While this might be a bad idea when we have a lot of traffic, the current
    -- implementation will replay until the traffic allows it.
    -- Some sort of action ordering/priority might be a good idea, but would complicate this
    -- even further, and it's already (too) complicated.
    -- In theory, all the threads reaching the exception will have the FIFO ordering with
    -- regard of the exception (raising) time.
    caughtAction :: m a
    caughtAction = action `catch` \(e :: HttpNetworkLayerException) ->
        case e of
            -- | Replay.
            HttpTooManyRequests _   ->
                delay >> runDispatch sc action

            -- | Rethrow.
            otherExceptions         ->
                throwM otherExceptions


