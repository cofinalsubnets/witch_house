module Gretel.Interface.Types
( Command
, CommandMap
) where

import Data.Map (Map)
import Gretel.World

type Command = [String] -> String -> World -> IO World
type CommandMap = Map String Command

