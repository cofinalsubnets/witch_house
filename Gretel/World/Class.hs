{-# LANGUAGE Rank2Types, FunctionalDependencies, ExistentialQuantification, UndecidableInstances #-}
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
) where

import System.IO (Handle)

type WT w = forall k. World w k => w -> (Bool,w)

data WS w r = forall k. World w k => WS { runWorld :: (w -> (r,w)) }

instance World w k => Monad (WS w) where
  return v = WS $ \w -> (v,w)
  s >>= t = WS $ \w0 ->
    let (r1,w1) = runWorld s w0
    in runWorld (t r1) w1

evalWorld :: WS w r -> w -> r
evalWorld ws = fst . runWorld ws

execWorld :: WS w r -> w -> w
execWorld ws = snd . runWorld ws

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

  exitsFor :: World w k => k -> w -> Maybe [(String,k)]
  exitsFor n w = do loc <- getLoc n w
                    getExits loc w

  from :: World w k => String -> k -> w -> Maybe k
  dir `from` k = \w -> exitsFor k w >>= lookup dir

  contents :: World w k => k -> w -> Maybe [k]
  contents k w
    | hasKey k w = Just $ [ c | c <- getKeys w, getLoc c w == Just k ]
    | otherwise = Nothing
                   
  goes :: World w k => k -> String -> WT w
  n `goes` dir = \w ->
    case exitsFor n w >>= lookup dir of
      Nothing -> (False,w)
      Just d  -> setLoc n d w

  takes :: World w k => k -> k -> WT w
  k1 `takes` k2 = \w ->
    if getLoc k1 w == getLoc k2 w
      then setLoc k2 k1 w
      else (False,w)

  enters :: World w k => k -> k -> WT w
  k1 `enters` k2 = k2 `takes` k1

  drops :: World w k => k -> k -> WT w
  k1 `drops` k2 = \w ->
    if getLoc k2 w == Just k1
      then case getLoc k1 w of
        Nothing -> (False,w)
        Just l  -> setLoc k2 l w
      else (False,w)

  leaves :: World w k => k -> k -> WT w
  k1 `leaves` k2 = k2 `drops` k1

