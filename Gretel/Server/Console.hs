module Gretel.Server.Console (startConsole) where

import Control.Concurrent.STM (atomically, TMVar, readTMVar)
import System.IO (hPutStrLn, hFlush, stdout)
import System.Exit (exitSuccess)
import Gretel.Server.Types
import Gretel.World
import Data.Maybe (catMaybes, isJust)

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

    ["clients"] -> do let ks = getKeys w
                          hs = filter (\k -> isJust $ getHandle k w) ks
                      putStrLn $ show (length hs) ++ " clients:"
                      mapM_ putStrLn ks
    ("broadcast":ss) -> do let ks = getKeys w
                               hs = catMaybes $ map (\k -> getHandle k w) ks
                               msg = unwords ss
                           mapM_ (\h -> hPutStrLn h msg) hs

    ["quit"] -> exitSuccess
    ["exit"] -> exitSuccess

    [] -> return ()
    (c:_) -> putStrLn $ "Unrecognized command: `" ++ c ++ "'"

  startConsole opts tmw

