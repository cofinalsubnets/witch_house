module Gretel.World.Base
( Object(..)
, World
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
) where


import System.IO
import Data.Map (Map)
import qualified Data.Map as M

type Key = String
type World = Map Key Object

-- | for object state
data Object = Object { name        :: Key
                     , description :: String
                     , location    :: Maybe Key
                     , exits       :: Map String Key
                     , client      :: Maybe Handle
                     , password    :: Maybe String
                     , isRoot      :: Bool
                     } deriving (Show,Eq)


mkWorld :: World
mkWorld = M.fromList []

mkObject :: Object
mkObject = Object { location    = Nothing
                  , exits       = M.fromList []
                  , name        = ""
                  , description = ""
                  , client      = Nothing
                  , password    = Nothing
                  , isRoot      = False
                  }

-- | retrieve an object from the world
get :: Key -> World -> Either String Object
get k w = case M.lookup k w of
  Nothing -> Left $ "There's no such thing as " ++ k
  Just o  -> Right o

get' :: Key -> World -> Object
get' k w = case get k w of
  Right o -> o
  Left  _ -> error "get' -- missing key"

set :: Object -> World -> World
set o = M.insert (name o) o

del :: Key -> World -> IO World
del k w = do
  case get k w of
    Left _ -> return w
    Right o -> case client o of
      Nothing -> return $ M.delete k w
      Just h -> do
        hClose h
        return $ M.delete k w

add :: Object -> World -> World
add o w = set o { location = Just . name $ root w } w

root :: World -> Object
root w = case filter isRoot (M.elems w) of
  []  -> error "root -- no root present"
  r:_ -> r

