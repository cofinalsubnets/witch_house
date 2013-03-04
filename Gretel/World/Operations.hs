{-# LANGUAGE Rank2Types, FlexibleContexts #-}
module Gretel.World.Operations
( makes
, adjoins
, deadends
, describes
, contents
, goes
, takes
, enters
, drops
, leaves
, hasKey
, exitsFor
, from
, getLoc
, setLoc
, getDesc
, setDesc
, getName
, setName
, getHandle
, setHandle
, unsetHandle
, addKey
, mkWorld
) where

import Data.Maybe
import Data.Map (Map)
import Gretel.World.Types

type NodeMap = Map String Node

makes :: String -> String -> WT NodeMap
n1 `makes` n2 = \w ->
  if not (hasKey n1 w)
    then (False,w)
    else setLoc n2 (fromJust $ getLoc n1 w) . snd $ addKey n2 w

adjoins :: World w k => k -> k -> String -> WT w
adjoins = addExit

deadends :: World w k => k -> String -> WT w
deadends = delExit

describes :: World w k => String -> k -> WT w
describes = flip setDesc

