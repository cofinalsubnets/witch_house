module Gretel.Command.Types
( Command
, CommandMap
) where

import Gretel.World (Name, WorldTransformer)
import Data.Map (Map)

type Command = Name -> [String] -> WorldTransformer String
type CommandMap = Map String Command

