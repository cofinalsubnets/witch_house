{-# LANGUAGE Rank2Types, FlexibleContexts #-}
-- | Aggregates and exports functions for interacting with the world.
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
, getObjs
, hasObj
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
, addObj
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
, addObj'
, delObj
, delObj'
, notify
, notifyObj
, kill
) where

import Data.Map (Map)
import Gretel.World.Types

type NodeMap = Map String Node

-- | Creates a new object, using the location of the first
-- argument as the initial location. The first argument is
-- intuitively the 'creator'; the second argument is the
-- name of the created object.
makes :: String -> String -> WT NodeMap
creator `makes` object = \w ->
  if not (hasObj creator w)
    then (False,w)
    else setLoc object (getLoc' creator w) . snd $ addObj object w

-- | alias for addExit
adjoins :: World w k c => k -> k -> String -> WT w
adjoins = addExit

-- | alias for delExit
deadends :: World w k c => k -> String -> WT w
deadends = delExit

-- | setDesc with the arguments reversed.
describes :: World w k c => String -> k -> WT w
describes = flip setDesc

