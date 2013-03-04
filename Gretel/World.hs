module Gretel.World
( Node(..)
, World
, Player(..)
, WorldTransformer
, WS(..)
, execWorld
, evalWorld
, module Gretel.World.Operations
) where


import Gretel.World.Operations
import Gretel.World.Types hiding (World)
import Gretel.World.Class()
import Data.Map (Map)

type World = Map String Node
type WorldTransformer a = World -> (a,World)

