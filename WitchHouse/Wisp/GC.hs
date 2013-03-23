module WitchHouse.Wisp.GC (gc, gcW) where

import WitchHouse.Types
import WitchHouse.Wisp.Core (toplevel, env)
import qualified Data.HashTable.IO as H
import Data.List
import Data.Graph
import WitchHouse.Wisp.TC (tc_func, tc_macro)
import qualified Data.Map as M

envGraph :: IO (Graph, (Vertex -> ((), Int, [Int])), (Int -> Maybe Vertex))
envGraph = do
  frames <- H.toList env
  return . graphFromEdges $ map (\(n,(bs,_)) -> ((), n, map frameNo $ filter (\s -> tc_func s || tc_macro s) (M.elems bs))) frames

gc :: IO ()
gc = do
  (g, v2i, i2v) <- envGraph
  let tv = case i2v toplevel of { Just v -> v; Nothing -> error "gc: rilly messed up env!" }
      getKey = (\(_,k,_) -> k) . v2i
      rs = reachable g tv
      vs = map getKey $ vertices g
      ks = map getKey rs
  mapM_ (H.delete env) (vs \\ ks)


gcW :: World -> IO ()
gcW w = do
  (g, v2i, i2v) <- envGraph
  let tv = case i2v toplevel of { Just v -> v; Nothing -> error "gc: rilly messed up env!" }
      getKey = (\(_,k,_) -> k) . v2i
      rs = reachable g tv
      vs = map getKey $ vertices g
      ks = map getKey rs ++ objIds w
  mapM_ (H.delete env) (vs \\ ks)
  where
    depth o = o:(contents o) ++ (concatMap depth $ contents o)
    breadth = concatMap depth
    objIds (f,c) = map objId $ breadth (f:c)

