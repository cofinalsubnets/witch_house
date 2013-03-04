{-# LANGUAGE Rank2Types, FunctionalDependencies, ExistentialQuantification #-}
module Gretel.World.Class
( WT
, World
, WS(..)
, execWorld
, evalWorld
, getLoc
, setLoc
, getName
, setName
, getDesc
, setDesc
, getHandle
, setHandle
, unsetHandle
, getExits
, addExit
, delExit
, getKeys
, hasKey
, contents
, exitsFor
, from
, goes
, takes
, drops
, enters
, leaves
, addKey
, mkWorld
, getName'
, getDesc'
, getExits'
, exitsFor'
, getHandle'
, getLoc'
, from'
, contents'
, setLoc'
, setName'
, setDesc'
, setHandle'
, unsetHandle'
, addExit'
, delExit'
, addKey'
) where

import System.IO (Handle)
import Data.Maybe (fromJust)

type WT w = forall k. World w k => w -> (Bool,w)

-- | State monad for world instances.
newtype WS w r = WS { runWorld :: (w -> (r,w)) }

instance Monad (WS w) where
  return v = WS $ \w -> (v,w)
  s >>= t = WS $ \w0 ->
    let (r1,w1) = runWorld s w0
    in runWorld (t r1) w1

-- | Run the monad with the given initial state, and return the `value' part
-- of the result.
evalWorld :: WS w r -> w -> r
evalWorld ws = fst . runWorld ws

-- | Run the monad with the given initial state, and return the `state' part
-- of the result
execWorld :: WS w r -> w -> w
execWorld ws = snd . runWorld ws

-- | Class defining an interface for `worlds' capable of storing program
-- state.
class (Ord k, Eq k) => World w k | w -> k where
  getLoc :: k -> w -> Maybe k
  setLoc :: k -> k -> WT w

  getName :: k -> w -> Maybe String
  setName :: k -> String -> WT w

  getDesc :: k -> w -> Maybe String
  setDesc :: k -> String -> WT w

  getHandle :: k -> w -> Maybe Handle
  setHandle :: k -> Handle -> WT w
  unsetHandle :: k -> WT w

  getExits :: k -> w -> Maybe [(String,k)]
  addExit  :: k -> k -> String -> WT w
  delExit  :: k -> String -> WT w

  getKeys :: w -> [k]
  addKey :: k -> WT w
  hasKey :: k -> w -> Bool

  mkWorld :: w

  exitsFor :: k -> w -> Maybe [(String,k)]
  exitsFor n w = do loc <- getLoc n w
                    getExits loc w

  from :: String -> k -> w -> Maybe k
  dir `from` k = \w -> exitsFor k w >>= lookup dir

  contents :: k -> w -> Maybe [k]
  contents k w
    | hasKey k w = Just $ [ c | c <- getKeys w, getLoc c w == Just k ]
    | otherwise = Nothing
                   
  goes :: k -> String -> WT w
  n `goes` dir = \w ->
    case exitsFor n w >>= lookup dir of
      Nothing -> (False,w)
      Just d  -> setLoc n d w

  takes :: k -> k -> WT w
  k1 `takes` k2 = \w ->
    if getLoc k1 w == getLoc k2 w
      then setLoc k2 k1 w
      else (False,w)

  enters :: k -> k -> WT w
  k1 `enters` k2 = k2 `takes` k1

  drops :: k -> k -> WT w
  k1 `drops` k2 = \w ->
    if getLoc k2 w == Just k1
      then case getLoc k1 w of
        Nothing -> (False,w)
        Just l  -> setLoc k2 l w
      else (False,w)

  leaves :: k -> k -> WT w
  k1 `leaves` k2 = k2 `drops` k1


-- Supplemental method versions. `Prime' getter methods will extract their
-- normal return value from Maybe (and raise an exception if the value is
-- Nothing). Prime setter methods will return a transformer function in the
-- WS state monad.

getName' :: World w k => k -> w -> String
getName' k = fromJust . getName k

getDesc' :: World w k => k -> w -> String
getDesc' k = fromJust . getDesc k

getHandle' :: World w k => k -> w -> Handle
getHandle' k = fromJust . getHandle k

getExits' :: World w k => k -> w -> [(String,k)]
getExits' k = fromJust . getExits k

exitsFor' :: World w k => k -> w -> [(String,k)]
exitsFor' k = fromJust . exitsFor k

getLoc' :: World w k => k -> w -> k
getLoc' k = fromJust . getLoc k

from' :: World w k => String -> k -> w -> k
from' s k = fromJust . from s k

contents' :: World w k => k -> w -> [k]
contents' k = fromJust . contents k

setLoc' :: World w k => k -> k -> WS w Bool
setLoc' k = WS . setLoc k

setName' :: World w k => k -> String -> WS w Bool
setName' k = WS . setName k

setDesc' :: World w k => k -> String -> WS w Bool
setDesc' k = WS . setDesc k

setHandle' :: World w k => k -> Handle -> WS w Bool
setHandle' k = WS . setHandle k

unsetHandle' :: World w k => k -> WS w Bool
unsetHandle' = WS . unsetHandle

addExit' :: World w k => k -> k -> String -> WS w Bool
addExit' k1 k2 = WS . addExit k1 k2

delExit' :: World w k => k -> String -> WS w Bool
delExit' k = WS . delExit k

addKey' :: World w k => k -> WS w Bool
addKey' = WS . addKey

