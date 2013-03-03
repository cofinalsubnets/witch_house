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
import System.IO (Handle)

type WorldTransformer a = World -> (a,World)
type World = Map Name Node

type Direction = String
type Name = String

data Node = Node { location    :: Maybe Name
                 , edges       :: Map Direction Name
                 , contents    :: Set Name
                 , name        :: Name
                 , description :: String
                 , properties  :: Set Property -- not really implemented yet
                 , handle      :: Maybe Handle
                 } deriving (Show, Eq)

data Property = Prop deriving (Show, Read, Eq, Ord)

instance Ord Node where
  (<) = (<) `on` name

