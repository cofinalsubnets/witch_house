module Gretel.World.Node
( mkNode
) where

import qualified Data.Set as S
import qualified Data.Map as M
import Gretel.World.Types

mkNode :: Node
mkNode = Node { location    = Nothing
              , edges       = M.fromList []
              , contents    = S.fromList []
              , name        = ""
              , description = ""
              , properties  = S.fromList []
              , handle      = Nothing
              }

