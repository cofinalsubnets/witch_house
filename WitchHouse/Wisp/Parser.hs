-- Wisp is a special-purpose Lisp for scripting object behaviour in witch_house.
module WitchHouse.Wisp.Parser (parseWisp) where

import WitchHouse.Types
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many, optional)

parseWisp :: String -> Either ParseError Sval
parseWisp = parse wisp ""
  where
    wisp = optional whitespace *> expr <* optional whitespace

    whitespace = many1 $ oneOf " \n\t\r"

    expr = nakedExpr <|> quotedExpr

    nakedExpr = sexp <|> atom
    quotedExpr = (\v -> Slist [Ssym "quote", v]) `fmap` (quote *> expr)
      where quote = char '\''

    sexp = fmap Slist $ char '(' *> optional whitespace *> expr `sepEndBy` whitespace <* char ')'

    atom = str <|> number <|> symbol <|> true <|> false

    str = Sstring `fmap` (char '"' *> many stringContents <* char '"')
      where stringContents = try (string "\\\"" >> return '"') <|> noneOf "\""

    symbol = Ssym `fmap` ((:) <$> symC <*> many (digit <|> symC))
      where symC = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ "_+-=*/.!?:")

    number = (Snum . read) `fmap` (try neg <|> pos)
      where pos = many1 digit
            neg = (:) <$> char '-' <*> pos

    true = Sbool `fmap` (try (string "#t") >> return True)
    false = Sbool `fmap` (try (string "#f") >> return False)

