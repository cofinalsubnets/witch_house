module Gretel.World.Types
( World
, WorldTransformer
, Name
, Node(..)
, Direction
, Property(..)
) where

import Data.Set (Set)
import Data.Map (Map)
import Data.Function (on)

type WorldTransformer a = World -> (a,World)
type World = Map Name Node

type Direction = String
type Name = String

data Node = Node { location    :: Maybe Name
                 , edges       :: Map Direction Name
                 , contents    :: Set Name
                 , name        :: Name
                 , description :: String
                 , properties  :: Set Property
                 } deriving (Show, Eq, Read)

data Property = Prop deriving (Show, Read, Eq, Ord)

instance Ord Node where
  (<) = (<) `on` name

