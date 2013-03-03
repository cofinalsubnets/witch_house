module Gretel.Interface.Types
( Command
, CommandMap
, Response(..)
, Scope(..)
) where

import Gretel.World (Name, WorldTransformer)
import Data.Map (Map)
import Gretel.Interface.Response

type Command = Name -> [String] -> WorldTransformer Response
type CommandMap = Map String Command

