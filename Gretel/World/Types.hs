{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Gretel.World.Types
( Node(..)
, module Gretel.World.Class
) where

import Gretel.World.Class
import Data.Map (Map)
import qualified Data.Map as M
import System.IO (Handle)

data Node = Node { name        :: String
                 , description :: String
                 , location    :: Maybe String
                 , edges       :: Map String String
                 , handle      :: Maybe Handle
                 } deriving (Show,Eq)

mkNode :: Node
mkNode = Node { location    = Nothing
              , edges       = M.fromList []
              , name        = ""
              , description = ""
              , handle      = Nothing
              }

update :: a -> Maybe a -> (Bool,a)
update g u = case u of Nothing -> (False,g)
                       Just g' -> (True,g')

instance World (Map String Node) String where
  getLoc n g = M.lookup n g >>= location

  setLoc n l g = update g $ do
    n' <- M.lookup n g
    _  <- M.lookup l g
    let n'' = n' { location = Just l }
    return $ M.insert n n'' g
  
  getName k g = M.lookup k g >>= return . name

  setName k t g = update g $ do
    n <- M.lookup k g
    let n' = n { name = t }
    return . M.insert t n' . M.delete k $ g

  getDesc k g = M.lookup k g >>= return . description

  setDesc k d g = update g $ do
    n <- M.lookup k g
    let n' = n { description = d }
    return $ M.insert k n' g

  getExits k g = M.lookup k g >>= return . M.toList . edges

  addExit k1 k2 d g = update g $ do
    n1 <- M.lookup k1 g
    _ <- M.lookup k2 g
    let n1' = n1 { edges = M.insert d k2 $ edges n1 }
    return $ M.insert k1 n1' g

  delExit k d g = update g $ do
    n <- M.lookup k g
    let n' = n { edges = M.delete d $ edges n }
    return $ M.insert k n' g

  getHandle k g = M.lookup k g >>= handle
  setHandle k h g = update g $ do
    n <- M.lookup k g
    let n' = n { handle = Just h }
    return $ M.insert k n' g

  getKeys = M.keys
  hasKey k = M.member k

  addKey k g = if M.member k g
    then (False,g)
    else let n = mkNode { name = k } in (True,M.insert k n g)

  mkWorld = M.fromList []

  unsetHandle k w = update w $ do
    n <- M.lookup k w
    let n' = n { handle = Nothing }
    return $ M.insert k n' w


