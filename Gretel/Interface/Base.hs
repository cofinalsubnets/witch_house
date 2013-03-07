module Gretel.Interface.Base
( Command
, CommandMap
, notify
) where

import Data.Map (Map)
import Gretel.World
import System.IO (hPutStrLn, hFlush)

type Command = [String] -> String -> World -> IO World
type CommandMap = Map String Command

notify :: Key -> String -> World -> IO World
notify k msg w = case get k w of
  Left _ -> return w
  Right o -> case client o of
    Nothing -> return w
    Just h -> hPutStrLn h msg >> hFlush h >> return w

