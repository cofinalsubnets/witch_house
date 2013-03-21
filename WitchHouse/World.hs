{-# LANGUAGE TupleSections #-}
module WitchHouse.World
( module WitchHouse.World.Core
, make
, mkObj
, invoke
, evalOn
, notify
, notifyExcept
, bootstrap
, world
) where

import WitchHouse.Types
import WitchHouse.World.Core
import WitchHouse.World.Interop
import WitchHouse.Wisp (toplevel, pushFrame)

import qualified Data.Map as M (fromList)
import qualified Data.Set as S (fromList)
import Data.Unique (newUnique)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

world :: IORef World
world = unsafePerformIO $ do
  root <- mkObj
  newIORef $ (root{start = True, name = "Root"}, [])

-- | Insert a new obj into the world with the given name.
make :: String -> World -> IO World
make n w = mkObj >>= \o -> return . zUp' $ zIns o {name = n, owners = S.fromList [objId $ focus w]} w

-- | Create a new obj. In IO because we need to grab a new Unique identifier.
mkObj :: IO Obj
mkObj = do
  i <- newUnique
  n <- pushFrame (M.fromList [], Just toplevel)
  return Obj { name        = ""
             , description = ""
             , objId       = i
             , exits       = M.fromList []
             , contents    = []
             , handle      = Nothing
             , password    = Nothing
             , owners      = S.fromList []
             , start       = False
             , frameId     = n
             }

