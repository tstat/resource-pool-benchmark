{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Benchmarks where

import AbstractPool
import Control.Concurrent
import Control.Exception
import Control.Monad
import Criterion
import TestConfig

{-# INLINE plentifulResources #-}
plentifulResources :: (AbstractPool -> TestConfig -> Benchmark) -> [Benchmark]
plentifulResources b =
  [ b resourcePool (TestConfig 5 5 25 60 2000),
    b resourcePoolWaiterPatch (TestConfig 1 25 25 60 2000)
  ]

{-# INLINE contentionTests #-}
contentionTests :: (AbstractPool -> TestConfig -> Benchmark) -> [Benchmark]
contentionTests b = do
  -- therad iterations
  let ti = 10
  -- thread count
  tc <- [500, 1000]
  pure $ bgroup ("t=" <> show tc) do
    -- pool size
    poolSize <- [25, 50]
    pure $
      bgroup ("c=" <> show poolSize) do
        -- thread delay per iteration
        td <- [2000, 10000]
        pure $
          bgroup ("d=" <> show td) $
            let rp =
                  [ b resourcePool (TestConfig sc connPerStripe tc ti td)
                    | sc <- [1, 5, 10, poolSize],
                      let (connPerStripe, m) = divMod poolSize sc,
                      m == 0
                  ]
                rpwp = [b resourcePoolWaiterPatch (TestConfig 1 poolSize tc ti td)]
             in rp ++ rpwp

{-# INLINE buildBenchmark #-}
buildBenchmark ::
  AbstractPool ->
  TestConfig ->
  IO a ->
  (a -> IO ()) ->
  (Int -> a -> IO ()) ->
  Benchmark
buildBenchmark k TestConfig {..} acquire release withResourceAction = k \poolName createPool destroyPool withResource ->
  let createEnv = do
        mainThreadId <- myThreadId
        pool <- createPool acquire release stripeCount 5 connCount
        waitVar <- newEmptyMVar
        doneVar <- newEmptyMVar
        let threadWork :: Int -> IO ()
            threadWork i = do
              withResource pool \c -> do
                withResourceAction withResourceSleep c
              case i < 1 of
                True -> putMVar doneVar ()
                False -> threadWork (i - 1)
        let makeThread iters =
              forkIO $
                (readMVar waitVar >> threadWork iters) `catch` \e -> case fromException @AsyncException e of
                  Just _ -> throwIO e
                  Nothing -> killThread mainThreadId >> throwIO e
        tids <- replicateM threadCount (makeThread threadIters)
        pure (const pool, waitVar, doneVar, tids)
      destroyEnv ~(pool, _, _, tids) = do
        mapM_ killThread tids
        destroyPool (pool ())
   in bench ("s=" <> show stripeCount <> " " <> poolName) $ perRunEnvWithCleanup createEnv destroyEnv \ ~(_, waitVar, doneVar, _) ->
        let collectDone :: Int -> IO ()
            collectDone 0 = pure ()
            collectDone d = do
              takeMVar doneVar
              collectDone (d - 1)
            act :: IO ()
            act = do
              putMVar waitVar ()
              collectDone threadCount
         in act
