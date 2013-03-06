module Gretel.Interface.Parser
( parseCommand
) where

import Gretel.World
import Gretel.Interface.Types
import Data.Char
import Data.List (isPrefixOf)
import qualified Data.Map as M

-- | Parse an input string into a command using the given
-- command map.
parseCommand :: CommandMap -> String -> String -> World -> IO World
parseCommand cm s = case tokenize s of
  Just (c:args) -> mLookup c cm $ args
  _ -> \_ w -> do notify (head $ words s) "Huh?" w
                  return w


mLookup :: String -> CommandMap -> Command
mLookup k cm = case M.lookup k cm of
  Just c -> c
  Nothing -> case filter (isPrefixOf k) (M.keys cm) of
    [m] -> cm M.! m
    [] -> \_ n w -> do notify n ("I don't know what `"++k++"' means.") w
                       return w
    ms -> \_ n w -> do notify n ("You could mean: " ++ show ms) w
                       return w
  

-- | TODO: Write tests for this. Make it generally suck less.
-- maybe use parsec
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

    isQuote c = c `elem` "`'\""
    isEscape c = c == '\\'
    escape _ [] _ = [Nothing]
    escape mode (c:cs) acc = mode cs (c:acc)

