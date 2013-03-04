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
, getKeys
, hasKey
, exitsFor
, from
, getLoc
, setLoc
, getDesc
, setDesc
, getName
, setName
, getClient
, getClient'
, setClient
, setClient'
, unsetClient
, unsetClient'
, addKey
, mkWorld
, getName'
, getDesc'
, getExits'
, exitsFor'
, getLoc'
, from'
, contents'
, setLoc'
, setName'
, setDesc'
, addExit'
, delExit'
, addKey'
, delKey
, delKey'
, notify
, notifyKey
, kill
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

adjoins :: World w k c => k -> k -> String -> WT w
adjoins = addExit

deadends :: World w k c => k -> String -> WT w
deadends = delExit

describes :: World w k c => String -> k -> WT w
describes = flip setDesc

