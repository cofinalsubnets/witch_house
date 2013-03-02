module Gretel.World
( module Gretel.World.Node
, module Gretel.World.Operations
, module Gretel.World.Types
, root
) where

import Gretel.World.Node
import Gretel.World.Operations
import Gretel.World.Types

import Data.Map (elems)
import Data.Maybe (isNothing)

-- | Return the root of the world.
-- TODO: enforce the constraint that a world have exactly one root with the
-- type system.
root :: World -> Node
root = head . filter (isNothing . location) . elems

