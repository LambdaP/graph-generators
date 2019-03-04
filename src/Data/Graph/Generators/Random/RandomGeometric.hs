{-# LANGUAGE BangPatterns #-}

module Data.Graph.Generators.Random.RandomGeometric
  ( randomGeometricGraphSquare
  , randomGeometricGraphTorus
  )
where

import           Control.Monad                  ( replicateM )
import           Control.Monad.Primitive
import           Data.Graph.Generators
import           Data.List                      ( tails )
import           System.Random.MWC

type Point = (Double, Double)

type GenM m = Gen (PrimState m)

squareDistance2 :: Point -> Point -> Double
squareDistance2 !(x1, y1) !(x2, y2) = x * x + y * y
 where
  x = x1 - x2
  y = y1 - y2

torusDistance2 :: Point -> Point -> Double
torusDistance2 !(x1, y1) !(x2, y2) = x * x + y * y
 where
  x = min (abs (x1 - x2)) (abs (1 - x1 + x2))
  y = min (abs (y1 - y2)) (abs (1 - y1 + y2))

genPoint :: (PrimMonad m) => GenM m -> m Point
genPoint !gen = do
  x <- uniformR (0, 1) gen
  y <- uniformR (0, 1) gen
  return (x, y)

randomGeometricGraph
  :: (PrimMonad m)
  => GenM m -- ^ The random number generator to use
  -> (Point -> Point -> Double) -- ^ The square distance to be used
  -> Int -- ^ The overall number of nodes (n)
  -> Double -- ^ The radius under which two nodes are connected
  -> m GraphInfo -- ^ The resulting graph
randomGeometricGraph _ _ n _ | n < 0 = error "Using negative graph order"
randomGeometricGraph _ _ _ r | r < 0 =
  error "Using negative construction radius"
randomGeometricGraph !gen sqd !n !r = do
  points <- replicateM n (genPoint gen)
  let
    namedPoints = zip [0 ..] points
    allEdges    = concat $ zipWith
      (\(i, p0) ps ->
        (i, i) : concat
          [ [(i, j), (j, i)] | (j, p) <- ps, j /= i, sqd p0 p < r * r ]
      )
      namedPoints
      (tails namedPoints)
  return (GraphInfo n allEdges)

randomGeometricGraphSquare
  :: (PrimMonad m)
  => GenM m -- ^ The random number generator to use
  -> Int -- ^ The overall number of nodes (n)
  -> Double -- ^ The radius under which two nodes are connected
  -> m GraphInfo -- ^ The resulting graph
randomGeometricGraphSquare g = randomGeometricGraph g squareDistance2

randomGeometricGraphTorus
  :: (PrimMonad m)
  => GenM m -- ^ The random number generator to use
  -> Int -- ^ The overall number of nodes (n)
  -> Double -- ^ The radius under which two nodes are connected
  -> m GraphInfo -- ^ The resulting graph
randomGeometricGraphTorus g = randomGeometricGraph g squareDistance2
