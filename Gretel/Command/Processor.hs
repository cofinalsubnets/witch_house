module Gretel.Command.Processor
( parseCommand
) where

import Gretel.Types
import Gretel.Command.Commands (huh)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad
import Data.Char

parseCommand :: CommandMap -> String -> WorldTransformer (IO String)
parseCommand cm s = case tokenize s of
  Nothing -> huh
  Just ss -> case ss of
    n:c:args -> case M.lookup c cm of
      Just comm -> comm n args
      Nothing -> huh
    _ -> huh

tokenize :: String -> Maybe [String]
tokenize s = mapM id $ unquoted s []
  where

    unquoted [] [] = []
    unquoted [] a = [Just $ reverse a]
    unquoted (c:cs) []
      | isSpace c = unquoted cs []
      | c == '"'  = quoted   cs []
      | otherwise = unquoted cs [c]
    unquoted (c:cs) a
      | isSpace c = (Just $ reverse a):(unquoted cs [])
      | c == '"'  = (Just $ reverse a):(quoted   cs [])
      | otherwise = unquoted cs (c:a)

    quoted [] a = [Nothing]
    quoted (c:cs) a
      | c == '"'  = if null a
                    then unquoted cs []
                    else (Just $ reverse a):(unquoted cs [])
      | otherwise = quoted cs (c:a)

