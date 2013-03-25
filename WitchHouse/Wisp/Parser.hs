-- Wisp is a special-purpose Lisp for scripting object behaviour in witch_house.
module WitchHouse.Wisp.Parser (parseWisp) where

import WitchHouse.Types
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many, optional)
import Data.ByteString.Char8 (pack)

parseWisp :: String -> Either ParseError Sval
parseWisp = parse wisp ""

wisp :: GenParser Char st Sval
wisp = optional whitespace *> expr <* optional whitespace
  where

    whitespace = many1 $ oneOf " \n\t\r"

    expr = nakedExpr <|> quotedExpr <|> quasiquotedExpr <|> splicedExpr

    nakedExpr = sexp <|> atom

    quotedExpr = (\v -> Slist [Ssym $ pack "quote", v]) `fmap` (quote *> expr)
      where quote = char '\''

    quasiquotedExpr = (\v -> Slist [Ssym $ pack "quasiquote", v]) `fmap` (qquote *> expr)
      where qquote = char '`'

    splicedExpr = (\v -> Slist [Ssym $ pack "splice", v]) `fmap` (splice *> expr)
      where splice = char ','

    sexp = fmap Slist $ char '(' *> optional wsOrComment *> expr `sepEndBy` wsOrComment <* char ')'

    wsOrComment = do whitespace
                     optional $ do char ';'
                                   many (noneOf "\n\r")
                                   char '\n' <|> char '\r'
                     optional whitespace

    atom = str <|> number <|> symbol <|> true <|> false

    escaped c r = try $ string ['\\',c] >> return r

    str = Sstring `fmap` (char '"' *> many stringContents <* char '"')
      where stringContents =  escaped '"' '"'
                          <|> escaped 'n' '\n'
                          <|> escaped 'r' '\r'
                          <|> escaped 't' '\t'
                          <|> escaped '\\' '\\'
                          <|> noneOf "\\\""

    symbol = (Ssym . pack) `fmap` ((:) <$> symC <*> many (digit <|> symC))
      where symC = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ "_+-=*/.!?:")

    number =  (Sfloat . read) `fmap` try dec
          <|> (Sfixn  . read) `fmap` neg
          <|> (Sfixn  . read) `fmap` pos

      where pos = many1 digit
            neg = (:) <$> char '-' <*> pos
            dec = (++) <$> (pos <|> neg) <*> ((:) <$> char '.' <*> pos)

    true = Sbool `fmap` (try (string "#t") >> return True)
    false = Sbool `fmap` (try (string "#f") >> return False)

