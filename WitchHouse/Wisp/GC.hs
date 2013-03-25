module WitchHouse.Wisp.GC (gc, gcW) where

import WitchHouse.Types
import Data.Graph
import qualified Data.Map as M
import Data.List
import WitchHouse.Wisp.Predicates
import WitchHouse.Wisp.Core
import qualified Data.HashTable.IO as H

-- | Create a graph representing the state of the lisp environment.
envGraph :: IO (Graph, (Vertex -> ((), Int, [Int])), (Int -> Maybe Vertex))
envGraph = do
  frames <- H.toList env
  return . graphFromEdges $ map es frames
  where es (n,(bs,p)) = ((), n, parent p ++ refs (M.elems bs))
        parent = maybe [] return
        refs = map frameNo . getRefs . Slist
        getRefs (Slist l) = filter (\o -> tc_func o || tc_macro o) l ++ concatMap getRefs l
        getRefs _ = []

-- | General case naive garbage collection. Drops all frames not reachable
-- from toplevel.
gc :: IO ()
gc = do
  (g, v2i, i2v) <- envGraph
  let tv = case i2v toplevel of { Just v -> v; Nothing -> error "gc: rilly messed up env!" }
      getKey = (\(_,k,_) -> k) . v2i
      rs = reachable g tv
      vs = map getKey $ vertices g
      ks = map getKey rs
  mapM_ (H.delete env) (vs \\ ks)


-- | GC algorithm modified to preserve frames belonging to or reachable from
-- object nodes.
gcW :: World -> IO ()
gcW w = do
  (g, v2i, i2v) <- envGraph
  let getKey = (\(_,k,_) -> k) . v2i
      getVec i = case i2v i of { Just v -> v; Nothing -> error "gcW: messed up env!" }
      rs = concatMap (reachable g) $ map getVec (toplevel:(objIds w))
      vs = map getKey $ vertices g
      ks = map getKey rs
  mapM_ (H.delete env) (vs \\ ks)
  where
    depth o = o:(contents o) ++ (concatMap depth $ contents o)
    breadth = concatMap depth
    objIds (f,c) = map objId $ breadth (f:c)

