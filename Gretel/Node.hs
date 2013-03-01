module Gretel.Node
( Direction
, Name
, Node(..)
, Property(..)
, mkNode
) where

import qualified Data.Set as S
import qualified Data.Map as M
import Gretel.Types

mkNode :: Node
mkNode = Node { location    = Nothing
              , edges       = M.fromList []
              , contents    = S.fromList []
              , name        = ""
              , description = ""
              , properties  = S.fromList []
              }

