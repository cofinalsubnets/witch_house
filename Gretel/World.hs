module Gretel.World
( Object(..)
, Client(..)
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
, notify
, kill
, makes
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


import System.IO
import Control.Concurrent
import Data.Map (Map)
import qualified Data.Map as M

type Key = String
type World = Map Key Object
type WorldTransformer = World -> Either String World

-- | for object state
data Object = Object { name        :: Key
                     , description :: String
                     , location    :: Maybe Key
                     , exits       :: Map String Key
                     , client      :: Maybe Client
                     , password    :: Maybe String
                     , isRoot      :: Bool
                     } deriving (Show,Eq)

data Client = Client { handle :: Handle
                     , thread :: ThreadId
                     } deriving (Show,Eq)

notify :: Key -> String -> World -> IO World
notify k msg w = case get k w of
  Left _ -> return w
  Right o -> case client o of
    Nothing -> return w
    Just (Client h _) -> hPutStrLn h msg >> hFlush h >> return w

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
      Just (Client h _) -> do
        hClose h
        return $ M.delete k w

add :: Object -> World -> World
add o w = set o { location = Just . name $ root w } w

root :: World -> Object
root w = case filter isRoot (M.elems w) of
  []  -> error "root -- no root present"
  r:_ -> r

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

exitsFor :: Key -> World -> Either String (Map String Key)
exitsFor n w = do obj  <- get n w
                  case location obj of
                    Nothing -> Left $ n ++ " is nowhere in particular."
                    Just l -> get l w >>= return . exits

contents :: Key -> World -> [Key]
contents k w = [ name c | c <- M.elems w, location c == Just k ]

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

