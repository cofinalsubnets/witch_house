module Gretel.World.Queries
( exitsFor
, contents
) where

import Gretel.World.Base
import Data.Map (Map)
import qualified Data.Map as M

exitsFor :: Key -> World -> Either String (Map String Key)
exitsFor n w = do obj  <- get n w
                  case location obj of
                    Nothing -> Left $ n ++ " is nowhere in particular."
                    Just l -> get l w >>= return . exits

contents :: Key -> World -> [Key]
contents k w = [ name c | c <- M.elems w, location c == Just k ]

