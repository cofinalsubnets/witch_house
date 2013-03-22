module WitchHouse.World.Global (world) where

import WitchHouse.Types
import WitchHouse.World.Core
import WitchHouse.Wisp (bind)
import System.IO.Unsafe
import Data.IORef
import Data.ByteString.Char8 (pack)

world :: IORef World
world = unsafePerformIO $ do
  root <- mkObj
  bind (objId root) (pack "*name*") (Sstring "Root")
  newIORef $ (root{start = True}, [])

