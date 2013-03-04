{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- | Defines the World instance used by the rest of the program.
module Gretel.World.Types
( Node(..)
, Player(..)
, module Gretel.World.Class
) where

import Gretel.World.Class
import Data.Map (Map)
import qualified Data.Map as M
import System.IO
import GHC.Conc
import System.IO.Unsafe (unsafePerformIO)

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

instance Client Player where
  notify (Player h _) msg = hPutStrLn h msg >> hFlush h
  -- | We fork off a new thread to close the original thread so clients don't
  -- hang if they 'kill' another active client. There must be a better way to
  -- do this though!
  kill (Player h t) = do _ <- forkIO $ hFlush h >> hClose h >> killThread t
                         return ()

mkNode :: Node
mkNode = Node { location    = Nothing
              , edges       = M.fromList []
              , name        = ""
              , description = ""
              , client      = Nothing
              }

update :: a -> Maybe a -> (Bool, a)
update g u = case u of Nothing -> (False, g)
                       Just g' -> (True, g')

instance World (Map Key Node) Key Player where
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

  setDesc k d g = update g $ do
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

  getClient k g = M.lookup k g >>= client
  setClient k c g = update g $ do
    n <- M.lookup k g
    let n' = n { client = Just c }
    return $ M.insert k n' g

  unsetClient k g = update g $ do
    n <- M.lookup k g
    let n' = n { client = Nothing }
    return $ M.insert k n' g

  getObjs = M.keys
  hasObj k = M.member k

  addObj k g = if M.member k g
    then (False, g)
    else let n = mkNode { name = k } in (True, M.insert k n g)

  mkWorld = M.fromList []

  -- TODO: make this better. it probably doesn't _need_ to be a pure function.
  delObj k w = let resp = (True, M.delete k w)
    in case getClient k w of
      Nothing -> resp
      Just c -> unsafePerformIO $ kill c >> return resp

