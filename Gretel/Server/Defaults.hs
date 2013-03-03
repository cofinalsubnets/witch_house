module Gretel.Server.Defaults (defaults) where

import Gretel.Server.Types
import Gretel.World

import Data.Map (fromList)
import System.IO (stderr)

defaults :: Options
defaults = Options { portNo = 10101
                   , maxClients = 10
                   , world = defaultWorld
                   , logHandle = stderr
                   , verbosity = V1
                   , console = False
                   }

defaultWorld :: World
defaultWorld = let r = mkNode { name = "Root of the World"
                              , description = "\"For the leaves to touch the sky, the roots much reach deep into hell.\"\n  --Thomas Mann"
                              }
  in addNode r $ fromList []

