{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import AbstractPool
import Benchmarks
import Control.Concurrent
import Criterion.Main
import TestConfig

main :: IO ()
main =
  let bmarks = (plentifulResources k ++ contentionTests k)
      k :: AbstractPool -> TestConfig -> Benchmark
      k abspool tc = buildBenchmark abspool tc (pure ()) pure (\s _c -> threadDelay s)
   in defaultMain bmarks
