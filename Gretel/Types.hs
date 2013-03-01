module Gretel.Types
( Node(..)
, Property(..)
, Command
, Response
, CommandMap
, Direction
, Name
, World
, WorldTransformer
) where

import Gretel.World.Types

type Command a = Name -> [String] -> WorldTransformer a
type Response = String
type CommandMap a = Map String (Command a)

