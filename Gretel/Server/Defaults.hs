module Gretel.Server.Defaults (defaults) where

import Gretel.Server.Types
import Gretel.World

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
defaultWorld = snd . addKey "Root of the World" $ mkWorld

