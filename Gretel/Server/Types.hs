module Gretel.Server.Types
( Req(..)
, Verbosity(..)
, Options(..)
) where

import System.IO (Handle)

import Gretel.World

data Req = Action { body   :: String
                  , client :: String
                  } |
           Login { client  :: String
                 , lHandle :: Handle
                 } |
           Logout String deriving (Show)

data Verbosity = V0 | V1 | V2 deriving (Show, Eq, Enum, Ord)

data Options = Options { portNo     :: Int
                       , maxClients :: Int              -- not implemented
                       , world      :: World
                       , logHandle  :: Handle
                       , verbosity  :: Verbosity
                       , console    :: Bool
                       }

