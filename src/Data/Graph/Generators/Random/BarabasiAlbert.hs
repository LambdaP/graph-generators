{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

{-|
 - Random graph generators using the generator algorithm
 - introduced by A. L. Barabási and R. Albert.

 - See.
 - A. L. Barabási and R. Albert "Emergence of scaling in
 -    random networks", Science 286, pp 509-512, 1999.
 -}
module Data.Graph.Generators.Random.BarabasiAlbert
  ( barabasiAlbertGraph
  , barabasiAlbertGraph'
  )
where

import           Control.Monad                  ( foldM )
import           Control.Monad.Primitive
import           Data.Graph.Generators
import           Data.IntMultiSet               ( IntMultiSet )
import qualified Data.IntMultiSet              as IntMultiSet
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IntSet
import           Data.List                      ( foldl' )
import           System.Random.MWC.Monad

data SizedIntMultiSet =
  SizedIntMultiSet Int
                   IntMultiSet

mkSizedIntMultiSet :: IntMultiSet -> SizedIntMultiSet
mkSizedIntMultiSet s = SizedIntMultiSet (IntMultiSet.size s) s

{-|
 - Select the nth element
 - from a multiset occur list,
 - treating it as a virtual large list.
 -
 - This is significantly faster
 - than building up the entire list
 - and selecting the nth element.
 -}
selectNth :: Int -> [(Int, Int)] -> Int
selectNth n ((a, c) : xs) | n <= c    = a
                          | otherwise = selectNth (n - c) xs
selectNth n _ =
  error
    $  "Can't select nth element - n is greater than list size (n="
    ++ show n
    ++ ", list empty)"

{-|
 - Select a single random element
 - from a multiset,
 - with precalculated size.
 -
 - Note that the given size
 - must be the total multiset size,
 - not the number of distinct elements
 - in the multiset.
 -}
selectRandomElement :: (PrimMonad m) => SizedIntMultiSet -> Mwc m Int
selectRandomElement (SizedIntMultiSet msSize ms) = do
  let msOccurList = IntMultiSet.toOccurList ms
  r <- uniformR (0, msSize - 1)
  return (selectNth r msOccurList)

{-|
 - Select n distinct random elements
 - from a multiset,
 - with precalculated size.
 -
 - This function will fail to terminate
 - if there are less than n distinct elements
 - in the multiset.
 -
 - This function accepts
 - a multiset with precomputed size
 - for performance reasons.
 -}
selectNDistinctRandomElements
  :: (PrimMonad m) => Int -> SizedIntMultiSet -> Mwc m [Int]
selectNDistinctRandomElements n t@(SizedIntMultiSet msSize ms)
  | n < msSize
  = fmap IntSet.toList (selectNDistinctRandomElementsWorker n t IntSet.empty)
  | n == msSize
  = (return . map fst . IntMultiSet.toOccurList) ms
  | otherwise
  = error
    $  "Attempting to select "
    ++ show n
    ++ " elements from a multiset of size "
    ++ show msSize
    ++ "."

{-|
 - Internal recursive worker for selectNDistinctRandomElements
 - Precondition:
 -   n > num distinct elems in multiset
 -   (not checked).
 -
 - The function does not terminate
 - if the precondition doesn't apply.
 -
 - This implementation is quite naive
 - and selects elements randomly
 - until the predefined number of elements are set.
 -}
selectNDistinctRandomElementsWorker
  :: (PrimMonad m) => Int -> SizedIntMultiSet -> IntSet -> Mwc m IntSet
selectNDistinctRandomElementsWorker 0 _ current = return current
selectNDistinctRandomElementsWorker n t current = do
  randomElement <- selectRandomElement t
  let currentWithRE = IntSet.insert randomElement current
  if randomElement `IntSet.member` current
    then selectNDistinctRandomElementsWorker n t current
    else selectNDistinctRandomElementsWorker (n - 1) t currentWithRE

{-|
 - Generate a random quasi-undirected Barabasi graph.
 -
 - Only one edge
 - (with nondeterministic direction)
 - is created between a node pair,
 - because adding the other edge direction
 - is easier than removing duplicates.
 -
 - Precondition (not checked): m <= n
 -
 - Modeled after NetworkX 1.8.1 barabasi_albert_graph()
 -}
barabasiAlbertGraph
  :: (PrimMonad m)
  => Int -- ^ The overall number of nodes (n)
  -> Int -- ^ The number of edges to create between a new and existing nodes (m)
  -> Mwc m GraphInfo -- ^ The resulting graph
barabasiAlbertGraph n m = do
  let initState = (IntMultiSet.empty, [0 .. m - 1], []) -- (Our state: repeated nodes, current targets, edges)
      -- From the final state, we only require the edge list
  (_, _, allEdges) <- foldM folder initState [m .. n - 1]
  return (GraphInfo n allEdges)
 where
  folder (repeatedNodes, targets, edges) curNode = do
    let newEdges = [ (curNode, j) | j <- targets ]
        newRepeatedNodes =
          foldl' (flip IntMultiSet.insert) repeatedNodes targets
        newRepeatedNodes' = IntMultiSet.insertMany curNode m newRepeatedNodes
    newTargets <- selectNDistinctRandomElements
      m
      (mkSizedIntMultiSet newRepeatedNodes)
    return (newRepeatedNodes', newTargets, edges ++ newEdges)

-- Strategy: Fold over the list, using a BarabasiState als fold state
-- Create new edges (for the current node)
-- Add nodes to the repeated nodes multiset
-- Select the new target set randomly from the repeated nodes
{-|
 - Like 'barabasiAlbertGraph',
 - but uses a newly initialized random number generator.
 -
 - See 'System.Random.MWC.withSystemRandom'
 - for details on how the generator is initialized.
 - By using this function,
 - you don't have to initialize the generator by yourself.
 - However, generator initialization is slow,
 - so reusing the generator is recommended.
 -
 - Usage example:
 -
 - > barabasiAlbertGraph' 10 5
 -}
barabasiAlbertGraph'
  :: Int -- ^ The number of nodes
  -> Int -- ^ The number of edges to create between a new and existing nodes (m)
  -> IO GraphInfo -- ^ The resulting graph
barabasiAlbertGraph' n m = runMwcWithSystemRandom (barabasiAlbertGraph n m)
