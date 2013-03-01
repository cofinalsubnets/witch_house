module Gretel.World
( runWorld
, testWorld
, module Gretel.World.Node
, module Gretel.World.Operations
, module Gretel.World.Types
) where

import Gretel.World.Node
import Gretel.World.Operations
import Gretel.World.Types

import qualified Data.Set as S
import qualified Data.Map as M

runWorld :: World -> [WorldTransformer a] -> [a]
runWorld w (t:ts) = let (r,w') = t w in r:(runWorld w' ts)
runWorld _ [] = []

testWorld :: World
testWorld = let root = mkNode { name = "Root of the World"
                              , description = "\"For the leaves to touch the sky, the roots much reach deep into hell.\"\n  --Thomas Mann"
                              , contents = S.fromList ["feivel"]
                              }
                feivel = mkNode { name = "feivel"
                                , description = "crusty kid w/ broken glasses"
                                , location = Just "Root of the World"
                                }
  in addNode feivel . addNode root $ M.fromList []

