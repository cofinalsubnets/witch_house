module Gretel.Server.Console (startConsole) where

import Control.Concurrent.STM (atomically, TMVar, readTMVar)
import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)
import Gretel.Server.Types
import Gretel.World
import Data.Maybe (isJust)

startConsole :: Options -> TMVar World -> IO ()
startConsole opts tmw = do
  putStr ">> "
  hFlush stdout
  cmd <- getLine
  w <- atomically $ readTMVar tmw
  case words cmd of
    ["save",file] -> do putStr $ "Writing world to " ++ file ++ "... "
                        writeFile file $ show w
                        putStrLn "done!"

    ["clients"] -> do let ks = getObjs w
                          cs = filter (\k -> isJust $ getClient k w) ks
                      putStrLn $ show (length cs) ++ " clients:"
                      mapM_ putStrLn cs
    ("broadcast":ss) -> do let ks = getObjs w
                               msg = unwords ss
                           mapM_ (\k -> notify k msg w) ks

    ["quit"] -> exitSuccess
    ["exit"] -> exitSuccess

    [] -> return ()
    (c:_) -> putStrLn $ "Unrecognized command: `" ++ c ++ "'"

  startConsole opts tmw

