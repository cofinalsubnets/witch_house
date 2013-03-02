module Gretel.Command.Processor
( parseCommand
) where

import Gretel.World (WorldTransformer)
import Gretel.Command.Types
import Data.Char
import qualified Data.Map as M

parseCommand :: CommandMap a -> String -> Maybe (WorldTransformer a)
parseCommand cm s = do
  toks <- tokenize s
  case toks of
    n:c:args -> do comm <- M.lookup c cm
                   return $ comm n args
    _ -> Nothing

tokenize :: String -> Maybe [String]
tokenize s = sequence $ unquoted s []
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

    quoted [] _ = [Nothing]
    quoted (c:cs) a
      | c == '"'  = if null a
                    then unquoted cs []
                    else (Just $ reverse a):(unquoted cs [])
      | otherwise = quoted cs (c:a)

