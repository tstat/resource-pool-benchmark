module TestConfig where

data TestConfig = TestConfig
  { stripeCount :: Int,
    connCount :: Int,
    threadCount :: Int,
    threadIters :: Int,
    withResourceSleep :: Int
  }
  deriving (Show)
