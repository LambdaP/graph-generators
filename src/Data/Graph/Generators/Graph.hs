{-|
 - Function to convert graph-generators 'Data.Graph.Generators.GraphInfo'
 - to Data.Graph.Graph data structures
 - as defined in the package containers.
 -}
module Data.Graph.Generators.Graph
  ( graphInfoToGraph
  )
where

import           Data.Graph                     ( Graph
                                                , buildG
                                                )
import           Data.Graph.Generators          ( GraphInfo )

graphInfoToGraph :: GraphInfo -> Graph
graphInfoToGraph (GraphInfo n es) = buildG 0 (n - 1) es
