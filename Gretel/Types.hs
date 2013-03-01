module Gretel.Types
( Node(..)
, Property(..)
, Command
, Response
, CommandMap
, Direction
, Name
, NodeOp2
, Predicate2
, World
, WorldTransformer
) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Function (on)
import Data.List (intercalate)
import qualified Data.Set as S (toList)

type Command  = Name -> [String] -> WorldTransformer (IO String)
type Response = String
type CommandMap = Map String Command

type WorldTransformer a = World -> (a,World)

type World = Map Name Node

type NodeOp2 = Name -> Name -> WorldTransformer Bool
type Predicate2 = Name -> Name -> World -> Bool

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

