-- | Types and low-level operations.
module Gretel.World.Base
( Object(..)
, Client(..)
, World
, WT
, Key
, get
, get'
, set
, del
, add
, root
, mkObject
, mkWorld
, M.member
, M.elems
, notify
, kill
) where

import System.IO
import Control.Concurrent
import Control.Monad (when)
import Data.Maybe (isNothing)
import Data.Map (Map)
import qualified Data.Map as M

type Key = String
type World = Map Key Object
type WT = World -> (World, Bool)

-- | for object state
data Object = Object { name        :: Key
                     , description :: String
                     , location    :: Maybe Key
                     , exits       :: Map String Key
                     , client      :: Maybe Client
                     } deriving (Show,Eq)

data Client = Client { handle :: Handle
                     , thread :: ThreadId
                     } deriving (Show,Eq)

notify :: Key -> String -> World -> IO ()
notify k msg w = case get k w >>= client of
  Nothing -> return ()
  Just (Client h _) -> hPutStrLn h msg >> hFlush h

kill :: Client -> IO ()
kill = hClose . handle

mkWorld :: World
mkWorld = M.fromList []

mkObject :: Object
mkObject = Object { location    = Nothing
                  , exits       = M.fromList []
                  , name        = ""
                  , description = ""
                  , client      = Nothing
                  }

-- | retrieve an object from the world
get :: Key -> World -> Maybe Object
get = M.lookup

get' :: Key -> World -> Object
get' = flip (M.!)

set :: Object -> World -> World
set o = M.insert (name o) o

del :: Key -> World -> IO World
del k w = do
  let cl = get k w >>= client
  case cl of Nothing -> return $ M.delete k w
             Just (Client h t) -> do
               hClose h
               mt <- myThreadId
               when (mt /= t) (killThread t)
               return $ M.delete k w

add :: Object -> World -> World
add o w = set o { location = Just . name $ root w } w

root :: World -> Object
root = head . filter (isNothing . location) . M.elems

