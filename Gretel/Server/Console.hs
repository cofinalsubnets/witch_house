module Gretel.Server.Console (startConsole) where

import GHC.Conc (ThreadId, killThread)
import Control.Concurrent.STM
import System.IO
import System.Exit (exitSuccess)
import Gretel.Server.Types
import Gretel.World
import qualified Data.Map as M

startConsole :: Options -> TMVar World -> TMVar ClientMap -> TQueue Req -> ThreadId -> ThreadId -> IO ()
startConsole opts tmw tmc q lid rid = do
  putStr ">> "
  hFlush stdout
  cmd <- getLine
  case words cmd of
    ["save",file] -> do w <- atomically $ readTMVar tmw
                        putStr $ "Writing world to " ++ file ++ "... "
                        writeFile file $ show w
                        putStrLn "done!"

    ["clients"] -> do cm <- atomically $ readTMVar tmc
                      let l = M.keys cm
                      putStrLn $ show (length l) ++ " clients:"
                      mapM_ putStrLn l

    ["shutdown"] -> do cm <- atomically $ readTMVar tmc
                       mapM_ (\h -> hFlush h >> hClose h) $ M.elems cm
                       killThread lid
                       killThread rid
                       exitSuccess

    ("broadcast":ms) -> do let msg = unwords ms
                           cm <- atomically $ readTMVar tmc
                           mapM_ (\h -> hPutStrLn h msg) $ M.elems cm
    _ -> putStrLn "Unrecognized command."
  startConsole opts tmw tmc q lid rid

