{-# LANGUAGE Rank2Types, FunctionalDependencies, ExistentialQuantification #-}
-- Defines the interface for the `world', the data structure that stores program state.
-- The server should get and set attributes of the world using methods of the World
-- typeclass.
module Gretel.World.Class
( WT
, World
, WS(..)
, Client
, execWorld
, evalWorld
, getLoc
, setLoc
, getName
, setName
, getDesc
, setDesc
, getClient
, getClient'
, setClient
, setClient'
, unsetClient
, unsetClient'
, notify
, kill
, notifyObj
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
, setLoc'
, setName'
, setDesc'
, addExit'
, delExit'
, addObj'
, delObj
, delObj'
) where

import Data.Maybe (fromJust)

-- Convenience type for boolean (succeed or fail) transformers
-- of world state.
type WT w = forall k. forall c. World w k c => w -> (Bool,w)

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

-- Interface for clients using the world. Provides methods to send messages and 
-- terminate connections.
class Client c where
  notify :: c -> String -> IO ()
  kill :: c -> IO ()

-- | Class defining an interface for `worlds' capable of storing program
-- state. Each world has a `object key' and a `client' type, which are uniquely
-- determined by the type of the world and the type of the key, respectively.
class (Ord k, Eq k, Client c) => World w k c | w -> k, k -> c where
  getLoc :: k -> w -> Maybe k
  setLoc :: k -> k -> WT w

  getName :: k -> w -> Maybe String
  setName :: k -> String -> WT w

  getDesc :: k -> w -> Maybe String
  setDesc :: k -> String -> WT w

  getClient   :: k -> w -> Maybe c
  setClient   :: k -> c -> WT w
  unsetClient :: k -> WT w

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

  -- | Convenience method for sending a notification directly
  -- to an object. (composes notify and getObj)
  notifyObj :: k -> String -> w -> IO ()
  notifyObj k msg w = case getClient k w of
    Nothing -> return ()
    Just c  -> notify c msg

-- Supplemental method versions. `Prime' getter methods will extract their
-- normal return value from Maybe (and raise an exception if the value is
-- Nothing). Prime setter methods will return a transformer function in the
-- WS state monad.

getName' ::  World w k c => k -> w -> String
getName' k = fromJust . getName k

getDesc' ::  World w k c => k -> w -> String
getDesc' k = fromJust . getDesc k

getClient' :: (Client c,  World w k c) => k -> w -> c
getClient' k = fromJust . getClient k

getExits' ::  World w k c => k -> w -> [(String,k)]
getExits' k = fromJust . getExits k

exitsFor' ::  World w k c => k -> w -> [(String,k)]
exitsFor' k = fromJust . exitsFor k

getLoc' ::  World w k c => k -> w -> k
getLoc' k = fromJust . getLoc k

from' ::  World w k c => String -> k -> w -> k
from' s k = fromJust . from s k

contents' ::  World w k c => k -> w -> [k]
contents' k = fromJust . contents k

setLoc' ::  World w k c => k -> k -> WS w Bool
setLoc' k = WS . setLoc k

setName' ::  World w k c => k -> String -> WS w Bool
setName' k = WS . setName k

setDesc' ::  World w k c => k -> String -> WS w Bool
setDesc' k = WS . setDesc k

setClient' ::  World w k c => k -> c -> WS w Bool
setClient' k = WS . setClient k

addExit' ::  World w k c => k -> k -> String -> WS w Bool
addExit' k1 k2 = WS . addExit k1 k2

delExit' ::  World w k c => k -> String -> WS w Bool
delExit' k = WS . delExit k

addObj' ::  World w k c => k -> WS w Bool
addObj' = WS . addObj

delObj' ::  World w k c => k -> WS w Bool
delObj' = WS . delObj

unsetClient' :: World w k c => k -> WS w Bool
unsetClient' = WS . unsetClient

