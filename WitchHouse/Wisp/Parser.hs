-- Wisp is a special-purpose Lisp for scripting object behaviour in witch_house.
module WitchHouse.Wisp.Parser (parseWisp) where

import WitchHouse.Types
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many, optional)
import Data.ByteString.Char8 (pack)

parseWisp :: String -> Either ParseError Val
parseWisp = parse wisp ""

wisp :: GenParser Char st Val
wisp = optional whitespace *> expr <* optional whitespace
  where

    whitespace = many1 $ oneOf " \n\t\r"

    expr = nakedExpr <|> quotedExpr <|> quasiquotedExpr <|> splicedExpr <|> mergedExpr

    nakedExpr = sexp <|> atom

    quotedExpr = (\v -> Lst [SFquote, v]) `fmap` (quote *> expr)
      where quote = char '\''

    quasiquotedExpr = (\v -> Lst [SFqq, v]) `fmap` (qquote *> expr)
      where qquote = char '`'

    splicedExpr = (\v -> Lst [SFsplice, v]) `fmap` (splice *> expr)
      where splice = char ','

    mergedExpr = (\v -> Lst [SFmerge, v]) `fmap` (splice *> expr)
      where splice = char '@'

    sexp = fmap Lst $ char '(' *> optional wsOrComment *> (fm <|> ls) <* char ')'
    fm = (:) <$> specialForm <*> ls
    ls = expr `sepEndBy` wsOrComment

    wsOrComment = do whitespace
                     optional $ do char ';'
                                   many (noneOf "\n\r")
                                   char '\n' <|> char '\r'
                     optional whitespace

    atom = str <|> number <|> symbol <|> true <|> false

    escaped c r = try $ string ['\\',c] >> return r

    str = Str `fmap` (char '"' *> many stringContents <* char '"')
      where stringContents =  escaped '"' '"'
                          <|> escaped 'n' '\n'
                          <|> escaped 'r' '\r'
                          <|> escaped 't' '\t'
                          <|> escaped '\\' '\\'
                          <|> noneOf "\\\""

    specialForm =  sf "if" SFif
               <|> sf "do" SFbegin
               <|> sf "qt" SFquote
               <|> sf "fn" SFlambda
               <|> sf "df" SFdef
               <|> sf "as" SFas
               <|> sf "mfn" SFmacro
               <|> sf "set" SFset
               <|> sf "undf" SFunset
               <|> sf "qq" SFqq
               <|> sf "sp" SFsplice
               <|> sf "msp" SFmerge

    sf s f = try $ string s >> whitespace >> return f

    symbol = (Sym . pack) `fmap` ((:) <$> symC <*> many (digit <|> symC))
      where symC = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ "_+-=*/.!?:<>&$^|{}[]%~")

    number =  (Flt . read) `fmap` try dec
          <|> (Int  . read) `fmap` try neg
          <|> (Int  . read) `fmap` pos

      where pos = many1 digit
            neg = (:) <$> char '-' <*> pos
            dec = (++) <$> (pos <|> neg) <*> ((:) <$> char '.' <*> pos)

    true = Bln `fmap` (try (string "#t") >> return True)
    false = Bln `fmap` (try (string "#f") >> return False)

