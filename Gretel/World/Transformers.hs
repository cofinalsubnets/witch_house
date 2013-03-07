module Gretel.World.Transformers
( makes
, takes
, goes
, drops
, leaves
, adjoins
, deadends
, enters
, describes
) where

import Gretel.World.Base
import qualified Data.Map as M

type WorldTransformer = World -> Either String World

-- | Creates a new object, using the location of the first
-- argument as the initial location. The first argument is
-- intuitively the 'creator'; the second argument is the
-- name of the created object.
makes :: String -> String -> WorldTransformer
creator `makes` object = \w -> do
  _ <- get creator w -- abort if creator doesn't exist
  let o = mkObject { location = Just (name $ root w)
                   , name = object
                   }
  return $ set o w

goes :: Key -> String -> WorldTransformer
n `goes` dir = \w -> do
  o <- get n w
  let xs = exits o
      dest = M.lookup dir xs
  case dest of Nothing -> Left $ "You can't go "++dir++"."
               Just d  -> do _ <- get d w -- fail if destination doesn't exist
                             return $ set o { location = dest } w

takes :: Key -> Key -> WorldTransformer
k1 `takes` k2 = \w -> do
  o1 <- get k1 w
  o2 <- get k2 w
  if o1 == o2
    then Left "You can't take yourself!"
    else if location o1 == location o2
      then return $ set o2 { location = Just k1 } w
      else Left $ "There is no "++k2++" here."

enters :: Key -> Key -> WorldTransformer
k1 `enters` k2 = k2 `takes` k1

drops :: Key -> Key -> WorldTransformer
k1 `drops` k2 = \w -> do
  o1 <- get k1 w
  o2 <- get k2 w
  if location o2 == Just k1
    then return $ set o2 { location = location o1 } w
    else Left "You can't drop what you don't have."

leaves :: Key -> Key -> WorldTransformer
k1 `leaves` k2 = k2 `drops` k1

adjoins :: Key -> Key -> String -> WorldTransformer
k1 `adjoins` k2 = \d w -> do
  o1 <- get k1 w
  _ <- get k2 w
  return $ set o1 { exits = M.insert d k2 (exits o1) } w

deadends :: Key -> String -> WorldTransformer
k `deadends` dir = \w -> do
  o <- get k w
  return $ set o { exits = M.delete dir (exits o) } w

describes :: String -> Key -> WorldTransformer
desc `describes` k = \w -> do
  o <- get k w
  return $ set o { description = desc } w

