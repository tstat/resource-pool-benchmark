{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import AbstractPool
import Benchmarks
import Criterion.Main
import Data.Int
import qualified Hasql.Connection as Hasql
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql
import TestConfig

main :: IO ()
main =
  let bmarks = (plentifulResources k ++ contentionTests k)
      k :: AbstractPool -> TestConfig -> Benchmark
      k abspool tc = buildBenchmark abspool tc (either (fail . show) pure =<< Hasql.acquire "host=localhost") Hasql.release \s c -> do
        _ <- runDB c (fromIntegral s / 1000000)
        pure ()
   in defaultMain bmarks

runDB :: Hasql.Connection -> Double -> IO Int64
runDB conn sleep = do
  Hasql.run (Hasql.statement sleep (Hasql.Statement "select 1::int8 where pg_sleep($1) is not null" encoder decoder True)) conn >>= \case
    Left err -> fail ("Hasql statement unexpectedly failed with error: " <> show err)
    Right x -> pure x
  where
    encoder = E.param (E.nonNullable E.float8)
    decoder = D.singleRow (D.column (D.nonNullable D.int8))
