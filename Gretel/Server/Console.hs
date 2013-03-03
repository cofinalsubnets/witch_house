module Gretel.Server.Console (startConsole) where

import GHC.Conc (ThreadId, killThread)
import Control.Concurrent.STM
import System.IO
import System.Exit (exitSuccess)
import Gretel.Server.Types
import Gretel.World
import qualified Data.Map as M
import Data.Maybe (catMaybes)

startConsole :: Options -> TMVar World -> TQueue Req -> ThreadId -> ThreadId -> IO ()
startConsole opts tmw q lid rid = do
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

    ["shutdown"] -> do let hs = catMaybes $ map handle (M.elems w)
                       mapM_ hClose hs
                       killThread lid
                       killThread rid
                       exitSuccess

    ("broadcast":ms) -> do let msg = unwords ms
                               hs  = catMaybes $ map handle (M.elems w)
                           mapM_ (\h -> hPutStrLn h msg) hs
    _ -> putStrLn "Unrecognized command."
  startConsole opts tmw q lid rid

