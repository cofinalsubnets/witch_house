-- Wisp is a special-purpose Lisp for scripting object behaviour in witch_house.
module WitchHouse.Wisp.Parser (parseWisp) where

import WitchHouse.Types
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)

parseWisp :: String -> Either ParseError Sval
parseWisp = parse wisp ""
  where
    wisp = many whitespace *> expr <* many whitespace

    whitespace = wsChar >> many wsChar
      where wsChar = oneOf " \n\t\r"

    expr = nakedExpr <|> quotedExpr

    nakedExpr = sexp <|> atom
    quotedExpr = (\v -> Slist [Ssym "quote", v]) `fmap` (quote *> expr)
      where quote = char '\''

    sexp = fmap Slist $ (char '(' >> many whitespace) *> expr `sepBy` whitespace <* (many whitespace >> char ')')

    atom = str <|> symbol <|> number <|> true <|> false

    str = Sstring `fmap` (char '"' *> many stringContents <* char '"')
      where stringContents = try (string "\\\"" >> return '"') <|> noneOf "\""

    symbol = Ssym `fmap` ((:) <$> symC <*> many (digit <|> symC))
      where symC = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ "_+-=*/.!?")

    number = (Snum . read) `fmap` ((:) <$> digit <*> many digit)

    true = Sbool `fmap` (try (string "#t") >> return True)
    false = Sbool `fmap` (try (string "#f") >> return False)

