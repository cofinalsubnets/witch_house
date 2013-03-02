{-# LANGUAGE TupleSections #-}
module Gretel.Command.Processor
( parseCommand
) where

import Gretel.World (WorldTransformer)
import Gretel.Command.Types
import Data.Char
import Data.List (isPrefixOf)
import qualified Data.Map as M

-- | Given a command map and a raw input string, returns a
-- world transformer.
parseCommand :: CommandMap -> String -> WorldTransformer String
parseCommand cm s = case tokenize s of
  Just (n:c:args) -> mLookup c cm n args
  _ -> ("Huh?",)


mLookup :: String -> CommandMap -> Command
mLookup k cm = case M.lookup k cm of
  Just c -> c
  Nothing -> case filter (isPrefixOf k) (M.keys cm) of
    [m] -> cm M.! m
    [] -> \_ _ -> ("I don't know what `"++k++"' means.",)
    ms -> \_ _ -> ("You could mean: " ++ show ms,)
  

-- | TODO: make this suck less.
tokenize :: String -> Maybe [String]
tokenize s = sequence $ unquoted s []
  where

    unquoted [] [] = []
    unquoted [] a = [Just $ reverse a]
    unquoted (c:cs) a
      | isSpace c && null a = unquoted cs a
      | isSpace c = (Just $ reverse a):(unquoted cs [])
      | isQuote c && null a = quoted c cs a
      | isQuote c = (Just $ reverse a):(quoted c cs [])
      | isEscape c = escape unquoted cs a
      | otherwise = unquoted cs (c:a)

    quoted _ [] _ = [Nothing]
    quoted q (c:cs) a
      | c == q && null a = unquoted cs []
      | c == q = (Just $ reverse a):(unquoted cs [])
      | isEscape c = escape (quoted q) cs a
      | otherwise = quoted q cs (c:a)

    isQuote c = c `elem` "'\""
    isEscape c = c == '\\'
    escape _ [] _ = [Nothing]
    escape mode (c:cs) acc = mode cs (c:acc)

