{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, Rank2Types, TupleSections #-}
-- | Defines the World instance used by the rest of the program.
module Gretel.World.Types
( Node(..)
, Player(..)
, module Gretel.World.Class
, kill
, getClient
, setClient
, dropClient
, notify
) where

import Gretel.World.Class
import Data.Map (Map)
import qualified Data.Map as M
import System.IO
import GHC.Conc

type Key = String

-- | stores object state
data Node = Node { name        :: Key
                 , description :: String
                 , location    :: Maybe Key
                 , edges       :: Map String Key
                 , client      :: Maybe Player
                 } deriving (Show,Eq)

-- | stores client state
data Player = Player { handle :: Handle
                     , thread :: ThreadId
                     } deriving (Show,Eq)

notify :: Key -> String -> Map Key Node -> IO ()
notify k msg w = case getClient k w of
  Nothing -> return ()
  Just (Player h _) -> hPutStrLn h msg >> hFlush h

kill :: Player -> IO ()
kill (Player h t) = (forkIO $ hClose h >> killThread t) >> return ()

mkNode :: Node
mkNode = Node { location    = Nothing
              , edges       = M.fromList []
              , name        = ""
              , description = ""
              , client      = Nothing
              }

update :: a -> Maybe a -> (a, Bool)
update g u = case u of Nothing -> (g, False)
                       Just g' -> (g', True)

getClient :: Key -> Map Key Node -> Maybe Player
getClient n g = M.lookup n g >>= client

setClient :: Key -> Player -> WT (Map Key Node)
setClient k p g = update g $ do
  n <- M.lookup k g
  let n' = n { client = Just p }
  return $ M.insert k n' g


-- TODO: see if this can be generalized.
dropClient :: Key -> Map Key Node -> IO (Map Key Node)
dropClient k g = do 
  case getClient k g of
    Nothing -> return g
    Just c  -> do kill c
                  let n  = g M.! k
                      n' = n { client = Nothing }
                      g' = M.insert k n' g
                  return g'

instance World (Map Key Node) Key where
  getLoc n g = M.lookup n g >>= location

  setLoc n l g = update g $ do
    n' <- M.lookup n g
    _  <- M.lookup l g -- so we get Nothing if the location isn't real.
    let n'' = n' { location = Just l }
    return $ M.insert n n'' g
  
  getName k g = M.lookup k g >>= return . name

  -- | There are four or five methods that are minor variations on this one.
  -- is there a generic way to update fields of record types? do you need
  -- template haskell for that?
  setName k t g = update g $ do
    n <- M.lookup k g
    let n' = n { name = t }
    return . M.insert t n' . M.delete k $ g

  getDesc k g = M.lookup k g >>= return . description

  setDesc k d = \g -> update g $ do
    n <- M.lookup k g
    let n' = n { description = d }
    return $ M.insert k n' g

  getExits k g = M.lookup k g >>= return . M.toList . edges

  addExit k1 k2 d g = update g $ do
    n1 <- M.lookup k1 g
    _ <- M.lookup k2 g -- see above
    let n1' = n1 { edges = M.insert d k2 $ edges n1 }
    return $ M.insert k1 n1' g

  delExit k d g = update g $ do
    n <- M.lookup k g
    let n' = n { edges = M.delete d $ edges n }
    return $ M.insert k n' g

  getObjs = M.keys
  hasObj k = M.member k

  addObj k g = if M.member k g
                 then (g, False)
                 else let n = mkNode { name = k } in (M.insert k n g, True)

  mkWorld = M.fromList []

  delObj k w = (M.delete k w, True)

