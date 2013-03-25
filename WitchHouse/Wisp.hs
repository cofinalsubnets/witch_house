module WitchHouse.Wisp
( evalWisp
, repl
, toplevel
, env
, stl
, bind
, unbind
, getFrame
, pushFrame
, dropFrame
, lookup
, gc
, gcW
) where

import WitchHouse.Types
import WitchHouse.Wisp.Core
import WitchHouse.Wisp.Parser
import WitchHouse.Wisp.STL
import WitchHouse.Wisp.GC

import System.IO
import System.IO.Error

import Control.Exception
import qualified Data.HashTable.IO as H

import Prelude hiding (lookup)

repl :: IO ()
repl = loop `catch` eof
  where loop = do gc
                  putStr "\n> "
                  hFlush stdout
                  l <- getLine
                  case l of
                    "\\env" -> do bl <- H.toList env
                                  putStrLn (show bl)
                                  loop
                    "" -> loop
                    _ -> do res <- evalWisp l toplevel
                            case res of
                              Left err -> putStr err >> loop
                              Right v -> putStr (show v) >> loop
        eof x = if isEOFError x then putStrLn "" else ioError x

evalWisp :: String -> Int -> IO (Either String Sval)
evalWisp s f = case parseWisp s of
  Right sv -> do v <- eval sv f
                 return v
  Left err -> return . Left $ show err

