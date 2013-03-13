module WitchHouse.World.Notify
( notify
, notifyExcept
) where

import WitchHouse.Types
import WitchHouse.World.Core
import Data.List
import System.IO

notify :: String -> World -> IO World
notify msg w = case handle . fst $ w of Nothing -> return w
                                        Just h -> hPutStrLn h msg >> hFlush h >> return w

notifyExcept :: String -> World -> IO World
notifyExcept msg w = case zUp w of Left _ -> return w
                                   Right w' -> mapM_ (notify msg) (delete w $ zDn w') >> return w

