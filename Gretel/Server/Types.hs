module Gretel.Server.Types
( Client(..)
, Req(..)
, Verbosity(..)
, Options(..)
, ClientMap
) where

import Data.Map (Map)
import Data.Function (on)
import System.IO (Handle)

import Gretel.World.Types

data Client = Client { cHandle :: Handle
                     , cName   :: String
                     } deriving (Show,Eq)

instance Ord Client where
  (>) = (>) `on` cName

data Req = Action { body   :: String
                  , client :: Client
                  } |
           Login { body    :: String
                 , lHandle :: Handle
                 } |
           Logout String deriving (Show)

type ClientMap = Map String Handle

data Verbosity = V0 | V1 | V2 deriving (Show, Eq, Enum, Ord)

data Options = Options { portNo     :: Int
                       , maxClients :: Int              -- not implemented
                       , world      :: World
                       , logHandle  :: Handle
                       , verbosity  :: Verbosity
                       , console    :: Bool
                       }

