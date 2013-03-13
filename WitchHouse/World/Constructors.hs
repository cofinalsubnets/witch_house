{-# LANGUAGE TupleSections #-}
module WitchHouse.World.Constructors
( make
, mkWorld
, mkObj
) where

import WitchHouse.World.Core
import WitchHouse.Wisp
import WitchHouse.Types
import Data.Unique
import qualified Data.Map as M

-- | Insert a new obj into the world with the given name.
make :: String -> World -> IO World
make n w = mkObj >>= \o -> return . zUp' $ zIns o {name = n} w

-- | Make a new world zipper with the supplied obj at the root.
mkWorld :: Obj -> World
mkWorld = (,[])

-- | Create a new obj. In IO because we need to grab a new Unique identifier.
mkObj :: IO Obj
mkObj = do
  i <- newUnique
  return Obj { name        = ""
             , description = ""
             , objId       = i
             , exits       = M.fromList []
             , contents    = []
             , handle      = Nothing
             , password    = Nothing
             , start       = False
             , bindings    = toplevel
             }

