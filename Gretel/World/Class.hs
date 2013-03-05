{-# LANGUAGE Rank2Types, FunctionalDependencies, FlexibleInstances, ExistentialQuantification #-}
-- Defines the interface for the `world', the data structure that stores program state.
-- The server should get and set attributes of the world using methods of the World
-- typeclass.
module Gretel.World.Class
( World
, WT
, WS(..)
, execWorld
, evalWorld
, getLoc
, setLoc
, getName
, setName
, getDesc
, setDesc
, getExits
, addExit
, delExit
, getObjs
, hasObj
, contents
, exitsFor
, from
, goes
, takes
, drops
, enters
, leaves
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
) where

import Data.Maybe (fromJust)
import Data.Monoid

type WT w = forall k. World w k => w -> (w, Bool)

newtype WS w r = WS { runWorld :: w -> (w,r) }

execWorld :: WS w r -> w -> w
execWorld ws = fst . runWorld ws

evalWorld :: WS w r -> w -> r
evalWorld ws = snd . runWorld ws

instance Monad (WS w) where
  return v = WS $ \w -> (w,v)
  s >>= t = WS $ \w0 ->
    let (w1,r1) = runWorld s w0
    in runWorld (t r1) w1

instance Monoid (WS w Bool) where
  mempty = return True
  mappend = (>>)

-- | Class defining an interface for `worlds' capable of storing program
-- state. Each world has a `object key' and a `client' type, which are uniquely
-- determined by the type of the world and the type of the key, respectively.
class (Ord k, Eq k) => World w k | w -> k where
  getLoc :: k -> w -> Maybe k
  setLoc :: k -> k -> WT w

  getName :: k -> w -> Maybe String
  setName :: k -> String -> WT w

  getDesc :: k -> w -> Maybe String
  setDesc :: k -> String -> WT w

  getExits :: k -> w -> Maybe [(String,k)]
  addExit  :: k -> k -> String -> WT w
  delExit  :: k -> String -> WT w

  getObjs :: w -> [k]
  addObj  :: k -> WT w
  hasObj  :: k -> w -> Bool
  delObj  :: k -> WT w

  mkWorld :: w

  exitsFor :: k -> w -> Maybe [(String,k)]
  exitsFor n w = do loc <- getLoc n w
                    getExits loc w

  from :: String -> k -> w -> Maybe k
  dir `from` k = \w -> exitsFor k w >>= lookup dir

  contents :: k -> w -> Maybe [k]
  contents k w
    | hasObj k w = Just $ [ c | c <- getObjs w, getLoc c w == Just k ]
    | otherwise = Nothing
                   
  goes :: k -> String -> WT w
  n `goes` dir = \w ->
    case exitsFor n w >>= lookup dir of
      Nothing -> (w, False)
      Just d  -> setLoc n d w

  takes :: k -> k -> WT w
  k1 `takes` k2 = \w ->
    if getLoc k1 w == getLoc k2 w
      then setLoc k2 k1 w
      else (w, False)

  enters :: k -> k -> WT w
  k1 `enters` k2 = k2 `takes` k1

  drops :: k -> k -> WT w
  k1 `drops` k2 = \w ->
    if getLoc k2 w == Just k1
      then case getLoc k1 w of
        Nothing -> (w, False)
        Just l  -> setLoc k2 l w
      else (w, False)

  leaves :: k -> k -> WT w
  k1 `leaves` k2 = k2 `drops` k1

getName' ::  World w k => k -> w -> String
getName' k = fromJust . getName k

getDesc' ::  World w k => k -> w -> String
getDesc' k = fromJust . getDesc k

getExits' ::  World w k => k -> w -> [(String,k)]
getExits' k = fromJust . getExits k

exitsFor' ::  World w k => k -> w -> [(String,k)]
exitsFor' k = fromJust . exitsFor k

getLoc' ::  World w k => k -> w -> k
getLoc' k = fromJust . getLoc k

from' ::  World w k => String -> k -> w -> k
from' s k = fromJust . from s k

contents' ::  World w k => k -> w -> [k]
contents' k = fromJust . contents k

