module Gretel.Interface.Types
( Command
, CommandMap
, Notification(..)
) where

import Gretel.World (Name, WorldTransformer)
import Data.Map (Map)

type Command = Name -> [String] -> WorldTransformer [Notification]
type CommandMap = Map String Command

data Notification = Notify { target  :: String
                           , message :: String
                           } deriving (Show,Eq)

