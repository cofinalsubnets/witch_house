module WitchHouse.Wisp.Repl (repl) where

import WitchHouse.Types
import WitchHouse.Wisp
import System.IO

repl :: IO ()
repl = repl' toplevel
  where repl' bs = do putStr "\n>> "
                      hFlush stdout
                      l <- getLine
                      case l of
                        ".env" -> putStr (show bs) >> repl' bs
                        ".quit" -> return ()
                        "" -> repl' bs
                        _ -> case run (runWisp l) bs of
                               (Left err, bs') -> do putStr err
                                                     repl' bs'
                               (Right v, bs') -> do putStr (show v)
                                                    repl' bs'

