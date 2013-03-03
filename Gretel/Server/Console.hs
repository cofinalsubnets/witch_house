module Gretel.Server.Console (startConsole) where

import Control.Concurrent.STM
import System.IO
import System.Exit (exitSuccess)
import Gretel.Server.Types
import Gretel.World

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

{-
    ["clients"] -> do cm <- atomically $ readTMVar tmc
                      let l = M.keys cm
                      putStrLn $ show (length l) ++ " clients:"
                      mapM_ putStrLn l
                      -}

    ["shutdown"] -> exitSuccess

    _ -> putStrLn "Unrecognized command."
  startConsole opts tmw

