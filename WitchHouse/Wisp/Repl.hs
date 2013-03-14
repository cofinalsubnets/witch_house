module WitchHouse.Wisp.Repl (repl) where

import WitchHouse.Types
import WitchHouse.Wisp
import System.IO

repl :: IO ()
repl = loop toplevel
  where loop bs = do putStr "\n>> "
                     hFlush stdout
                     l <- getLine
                     case l of
                       "\\env" -> putStr (show bs) >> loop bs
                       "\\quit" -> return ()
                       "" -> loop bs
                       _ -> case run (runWisp l) bs of
                              (Left err, bs') -> putStr err >> loop bs'
                              (Right v, bs') -> putStr (show v) >> loop bs'

