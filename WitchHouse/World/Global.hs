module WitchHouse.World.Global (world) where

import WitchHouse.Types
import WitchHouse.World.Core
import System.IO.Unsafe
import Data.IORef

world :: IORef World
world = unsafePerformIO $ do
  root <- mkObj
  newIORef $ (root{start = True, name = "Root"}, [])

