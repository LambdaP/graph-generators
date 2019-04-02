module Data.Graph.Generators.Internal
  ( randomFilter
  , randomContext
  )
where

import           Control.Monad                  ( filterM )
import           Control.Monad.Primitive
import           Data.Graph.Generators          ( GraphContext(..) )
import           System.Random.MWC.Monad

randomFilter :: (PrimMonad m) => Double -> [a] -> Mwc m [a]
randomFilter p = filterM (\_ -> fmap (<= p) uniform)

randomContext
  :: (PrimMonad m)
  => Int -- ^ Identifier of the context's central node
  -> [Int] -- ^ Nodes to potentially connect to the central node
  -> Double -- ^ The probability for any pair of nodes to be connected
  -> Mwc m GraphContext -- ^ The resulting graph
randomContext node allNodes p =
  fmap (\i -> GraphContext i node) endpoints <*> endpoints
  where endpoints = randomFilter p allNodes
