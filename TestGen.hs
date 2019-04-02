{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Data.Graph.Generators          ( GraphInfo(..) )
import           Data.Graph.Generators.Random.BarabasiAlbert
                                                ( barabasiAlbertGraph' )
import           Data.Graph.Generators.Random.ErdosRenyi
                                                ( erdosRenyiGraph' )
import           Data.Graph.Generators.Random.WattsStrogatz
                                                ( wattsStrogatzGraph' )
import           System.Directory
import           System.Environment
import           System.IO

-- import           System.Random.MWC              ( createSystemRandom )
d, n, m0 :: Int
d = 20

n = 1000

p, b :: Double
p = toDbl d / toDbl n

b = 0.28

m0 = 10

toDbl :: Int -> Double
toDbl = fromInteger . toInteger

sampleDir :: String
sampleDir = "dist/build/samples"

writeGraph :: FilePath -> GraphInfo -> IO ()
writeGraph path (GraphInfo {..}) = withFile
  path
  WriteMode
  (\handle -> do
    hPutStrLn handle "strict graph {"
    mapM_ (hPutStrLn handle)
          [ (show x) ++ " -- " ++ (show y) | (x, y) <- graphInfoEdges ]
    hPutStrLn handle "}"
  )

main :: IO ()
main = do
  _  <- getArgs
  -- gen <- createSystemRandom
  eG <- erdosRenyiGraph' n p
  wG <- wattsStrogatzGraph' n d b
  bG <- barabasiAlbertGraph' n d
  createDirectoryIfMissing True "dist/build/samples"
  writeGraph (sampleDir ++ "/ErdosRenyiGraph.dot")     eG
  writeGraph (sampleDir ++ "/WattsStrogatz.dot")       wG
  writeGraph (sampleDir ++ "/BarabasiAlbertGraph.dot") bG
  putStrLn "Sample DOT files have been generated in ./dist/build/samples"
