module WitchHouse.Version (version) where

import Data.Version

version :: String
version = showVersion $ Version [0,0,0] ["pre-pre-alpha"]

