module Gretel.Interface.Types
( Command
, CommandMap
, Notification(..)
) where

import Data.Map (Map)
import Gretel.World (WorldTransformer)

type Command = String -> [String] -> WorldTransformer [Notification]
type CommandMap = Map String Command

data Notification = Notify { target  :: String
                           , message :: String
                           } deriving (Show,Eq)

