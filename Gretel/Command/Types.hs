module Gretel.Command.Types
( Command
, CommandMap
) where

import Gretel.World (Name, WorldTransformer)
import Data.Map (Map)

type Command a = Name -> [String] -> WorldTransformer a
type CommandMap a = Map String (Command a)

