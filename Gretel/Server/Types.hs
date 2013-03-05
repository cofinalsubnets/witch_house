module Gretel.Server.Types
( Verbosity(..)
, Options(..)
) where

import System.IO (Handle)

import Gretel.World

data Verbosity = V0 | V1 | V2 deriving (Show, Eq, Enum, Ord)

data Options = Options { portNo     :: Int
                       , maxClients :: Int              -- not implemented
                       , world      :: World
                       , logHandle  :: Handle
                       , verbosity  :: Verbosity
                       }

