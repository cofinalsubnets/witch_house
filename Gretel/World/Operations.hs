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
, setClient
, dropClient
, addObj
, mkWorld
, getName'
, getDesc'
, getExits'
, exitsFor'
, getLoc'
, from'
, contents'
, delObj
, notify
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
    then (w, False)
    else let (w',_) = addObj object w
         in setLoc object (getLoc' creator w) w'

-- | alias for addExit
adjoins :: World w k => k -> k -> String -> WT w
adjoins = addExit

-- | alias for delExit
deadends :: World w k => k -> String -> WT w
deadends = delExit

-- | setDesc with the arguments reversed.
describes :: World w k => String -> k -> WT w
describes = flip setDesc

