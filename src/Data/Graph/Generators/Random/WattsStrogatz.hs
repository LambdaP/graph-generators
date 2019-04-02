{-# LANGUAGE ScopedTypeVariables #-}

{-|
  Implementations of binomially random graphs, as described by Erdős and Rényi.

  Graphs generated using this method have a constant edge probability between two nodes.

  See Erdős and A. Rényi, On Random Graphs, Publ. Math. 6, 290 (1959).

  graph-generators copyright:
    Copyright (C) 2014 Uli Köhler

  NetworkX copyright:
    Copyright (C) 2004-2010 by
    Aric Hagberg <hagberg@lanl.gov>
    Dan Schult <dschult@colgate.edu>
    Pieter Swart <swart@lanl.gov>
    All rights reserved.
    BSD license.
-}
module Data.Graph.Generators.Random.WattsStrogatz
  ( wattsStrogatzGraph
  , wattsStrogatzGraph'
        -- ** Graph component generators
  , wattsStrogatzContext
  )
where

import           Control.Monad                  ( foldM )
import           Control.Monad.Loops            ( iterateWhile )
import           Control.Monad.Primitive
import           Data.Graph.Generators
import           Data.Graph.Generators.Internal ( randomContext
                                                , randomFilter
                                                )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           System.Random.MWC.Monad

type Node = Int

type Edge = (Node, Node)

type MapGraph = IntMap (Set Node)

{-|
    Generate a small-world context using the Watts-Strogatz method.

    NOTE: this used to have the exact same code
    as Data.Graph.Generators.ErdosRenyi.erdosRenyiContext,
    so the actual code
    was moved to Data.Graph.Internal
    after refactorization.

    See 'wattsStrogatzGraph' for a detailed algorithm description.

    Example usage, using a truly random generator:

    > import System.Random.MWC
    > gen <- withSystemRandom . asGenIO $ return
    >
-}
wattsStrogatzContext
  :: (PrimMonad m)
  => Int -- ^ Identifier of the context's central node
  -> [Int] -- ^ The algorithm will generate random edges to those nodes
                      --   from or to the given node
  -> Double -- ^ The probability for any pair of nodes to be connected
  -> Mwc m GraphContext -- ^ The resulting graph (IO required for randomness)
wattsStrogatzContext = randomContext

{-|
    Generate a unlabelled undirected random graph using the Algorithm introduced by
    WattsStrogatz.

    Note that self-loops with also be generated with probability p.

    This algorithm runs in O(kn).

    The generated nodes are identified by [0..n-1].

    Example usage, using a truly random generator:

    > import System.Random.MWC
    > gen <- withSystemRandom . asGenIO $ return
    > wattsStrogatzGraph gen 1000 10 0.6
    ...

-}
wattsStrogatzGraph
  :: forall m
   . (PrimMonad m)
  => Int -- ^ n, The number of nodes
  -> Int -- ^ k, the size of the neighborhood / degree (should be even)
  -> Double -- ^ \beta, The probability of a forward edge getting rewired
  -> Mwc m GraphInfo -- ^ The resulting graph
wattsStrogatzGraph n k β = do
  let lattice = ringLattice n (k `div` 2)
  rewired <- foldM rewireForwardNeighborhood lattice [0 .. n - 1]
  let allEdges = [ (i, j) | (i, js) <- Map.toList rewired, j <- Set.toList js ]
  return (GraphInfo n allEdges)
 where
  rewireForwardNeighborhood :: MapGraph -> Node -> Mwc m MapGraph
  rewireForwardNeighborhood graph node = do
    let forward = forwardNeighbors n (k `div` 2) node
    forward' <- randomFilter β forward
    foldM (rewireEdge node) graph forward'
  rewireEdge :: Node -> MapGraph -> Node -> Mwc m MapGraph
  rewireEdge node graph oldNeighbor = do
    newNeighbor <- randomNonNeighbor node graph
    return
      ((insertEdge (node, newNeighbor) . removeEdge (node, oldNeighbor)) graph)
  randomNonNeighbor :: Int -> MapGraph -> Mwc m Int
  randomNonNeighbor node graph = iterateWhile (\j -> node == j || isNeighbor j)
    $ uniformR (0, n - 1)
    where isNeighbor j = isEdge (node, j) graph || isEdge (j, node) graph

{-|
 - Constructs the regular ring lattice
 - with n nodes
 - where each node has 2*halfK neighbors.
 -
 - The nodes are labeled 0 through n-1.
 -}
ringLattice :: Int -> Int -> MapGraph
ringLattice n halfK = Map.fromList
  [ (i, Set.fromList (forwardNeighbors n halfK i)) | i <- [0 .. n - 1] ]

forwardNeighbors :: Int -> Int -> Int -> [Int]
forwardNeighbors n halfK i = [ (i + j) `mod` n | j <- [1 .. halfK] ]

isEdge :: Edge -> MapGraph -> Bool
isEdge (i, j) graph = case Map.lookup i graph of
  Just neighbors -> Set.member j neighbors
  _              -> False

removeEdge :: Edge -> MapGraph -> MapGraph
removeEdge (k, v) graph = case Map.lookup k graph of
  Just ns -> Map.insert k (Set.delete v ns) graph
  Nothing -> Map.insert k (Set.empty) graph

insertEdge :: Edge -> MapGraph -> MapGraph
insertEdge (k, v) graph = case Map.lookup k graph of
  Just ns -> Map.insert k (Set.insert v ns) graph
  Nothing -> Map.insert k (Set.singleton v) graph

{-|
    Like 'wattsStrogatzGraph', but uses a newly initialized random number generator.

    See 'System.Random.MWC.withSystemRandom' for details on how the generator is
    initialized.

    By using this function, you don't have to initialize the generator by yourself,
    however generator initialization is slow, so reusing the generator is recommended.

    Usage example:

    > wattsStrogatzGraph' 1000 10 0.6
-}
wattsStrogatzGraph'
  :: Int -- ^ n, The number of nodes
  -> Int -- ^ k, the size of the neighborhood / degree (should be even)
  -> Double -- ^ \beta, The probability of a forward edge getting rewritten
  -> IO GraphInfo -- ^ The resulting graph (IO required for randomness)
wattsStrogatzGraph' n k p = runMwcWithSystemRandom (wattsStrogatzGraph n k p)
