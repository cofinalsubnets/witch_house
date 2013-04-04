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
, load
) where

import WitchHouse.Types
import WitchHouse.Wisp.Core
import WitchHouse.Wisp.Parser
import WitchHouse.Wisp.GC
import WitchHouse.Wisp.STL

import System.IO
import System.IO.Error

import Control.Exception
import qualified Data.HashTable.IO as H

import Prelude hiding (lookup)

load :: Int -> Module -> IO ()
load f (bs,w) = do
  mapM_ (\(k,v) -> bind f k v) bs
  evalWisp f w
  return ()

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
                    _ -> do res <- evalWisp toplevel l
                            case res of
                              Left err -> putStr err >> loop
                              Right v -> putStr (show v) >> loop
        eof x = if isEOFError x then return () else ioError x

evalWisp :: Int -> String -> IO (Either String Val)
evalWisp f s = case parseWisp s of
  Right sv -> do v <- eval sv f
                 return v
  Left err -> return . Left $ show err

