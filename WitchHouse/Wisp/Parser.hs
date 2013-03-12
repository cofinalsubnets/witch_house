module WitchHouse.Wisp.Parser (parseWisp) where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import WitchHouse.Wisp.Types

parseWisp = parse sexp ""

sexp = fmap Slist $ char '(' *> expr `sepBy` whitespace <* char ')'

whitespace = wsChar >> many wsChar
  where wsChar = oneOf " \n\t\r"

expr = sexp <|> atom

atom = str <|> symbol <|> number <|> true <|> false

str = Sstring `fmap` (char '"' *> many stringContents <* char '"')
  where stringContents = try (string "\\\"" >> return '"') <|> noneOf "\""

true = Sbool `fmap` (try (string "#t") >> return True)
false = Sbool `fmap` (try (string "#f") >> return False)

nonNum = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ "_+-=*/.'")

number = (Snum . read) `fmap` ((:) <$> digit <*> many digit)

symbol = Ssym `fmap` ((:) <$> nonNum <*> many (digit <|> nonNum))

