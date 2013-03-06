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
, exitsFor
) where

import Data.Map (Map)
import qualified Data.Map as M
import Gretel.World.Base

-- | Creates a new object, using the location of the first
-- argument as the initial location. The first argument is
-- intuitively the 'creator'; the second argument is the
-- name of the created object.
makes :: String -> String -> WT
creator `makes` object = \w ->
  if not $ member creator w
    then (w, False)
    else let o = mkObject { location = Just (name $ root w)
                          , name = object
                          }
         in (set o w, True)

exitsFor :: Key -> World -> Maybe (Map String Key)
exitsFor n w = do obj  <- get n w
                  loc  <- location obj
                  loc' <- get loc w
                  return $ exits loc'

contents :: Key -> World -> [Key]
contents k w = [ name c | c <- M.elems w, location c == Just k ]

update :: World -> Maybe World -> (World, Bool)
update w mw = case mw of Nothing -> (w, False)
                         Just w' -> (w', True)

goes :: Key -> String -> WT
n `goes` dir = \w -> update w $ do
  o <- get n w
  xs <- exitsFor n w
  dest <- M.lookup dir xs
  return $ set o { location = Just dest } w

takes :: Key -> Key -> WT
k1 `takes` k2 = \w -> update w $ do
  o1 <- get k1 w
  o2 <- get k2 w
  if location o1 == location o2
    then return $ set o2 { location = Just k1 } w
    else Nothing

enters :: Key -> Key -> WT
k1 `enters` k2 = k2 `takes` k1

drops :: Key -> Key -> WT
k1 `drops` k2 = \w -> update w $ do
  o1 <- get k1 w
  o2 <- get k2 w
  _ <- location o1 -- the root can't drop things
  if location o2 == Just k1
    then return $ set o2 { location = location o1 } w
    else Nothing

leaves :: Key -> Key -> WT
k1 `leaves` k2 = k2 `drops` k1

adjoins :: Key -> Key -> String -> WT
k1 `adjoins` k2 = \d w -> update w $ do
  o1 <- get k1 w
  _ <- get k2 w
  return $ set o1 { exits = M.insert d k2 (exits o1) } w

deadends :: Key -> String -> WT
k `deadends` dir = \w -> update w $ do
  o <- get k w
  return $ set o { exits = M.delete dir (exits o) } w
  

describes :: String -> Key -> WT
desc `describes` k = \w -> update w $ do
  o <- get k w
  return $ set o { description = desc } w

