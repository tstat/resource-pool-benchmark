{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}

module AbstractPool where

import qualified "resource-pool" Data.Pool as RP
import qualified "resource-pool-waiter-patch" Data.Pool as RPWP
import Data.Time (NominalDiffTime)

type AbstractPool =
  ( forall a r.
    ( forall pool.
      String ->
      (IO a -> (a -> IO ()) -> Int -> NominalDiffTime -> Int -> IO (pool a)) ->
      (pool a -> IO ()) ->
      (forall b. pool a -> (a -> IO b) -> IO b) ->
      r
    ) ->
    r
  )

{-# INLINE resourcePool #-}
resourcePool :: AbstractPool
resourcePool k = k "resource-pool" RP.createPool RP.destroyAllResources RP.withResource

{-# INLINE resourcePoolWaiterPatch #-}
resourcePoolWaiterPatch :: AbstractPool
resourcePoolWaiterPatch k = k "resource-pool-patched" RPWP.createPool RPWP.destroyAllResources RPWP.withResource
