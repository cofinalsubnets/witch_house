module WitchHouse.Wisp.Test (testLib) where

import WitchHouse.Types
import WitchHouse.Wisp.Predicates
import Data.ByteString.Char8 (pack)

testLib :: Module
testLib = (lib,defs)
  where
    lib = [ (pack "print", t_print) ]
    defs = unlines $
      [ "(do"
      , "  (dfn colorize (color s)"
      , "    (str color s \"\x1b[0m\"))"

      , "  (df red  \"\x1b[31m\")"
      , "  (df green  \"\x1b[32m\")"
      , "  (dfn println (s) (print (str s \"\\n\")))"
      , "  (dfm test (suite & ts)"
      , "    (if (list? suite)"
      , "        (set ts (cons suite ts))"
      , "        (println (str \"Running \" suite \" ...\")))"
      , "    (dfn pass () (print (colorize green \".\")))"
      , "    (dfn fail () (print (colorize red \"F\")))"
      , "    (df failures '())"
      , "    (map (fn (x)"
      , "           (let (((doc t) x))"
      , "             (if (eval t)"
      , "                 (pass)"
      , "                 (do (set failures (cons doc failures))"
      , "                     (fail)))))"
      , "         ts)"
      , "    (println \"\")"
      , "    (if (null? failures)"
      , "      (do (println \"ok!\") #t)"
      , "      (do"
      , "        (println (str (length failures) \" failure(s): \"))"
      , "        (map println failures)"
      , "        #f)))"
      , ")"
      ]

t_print = Primitive $ taking (Exactly 1 strings) $ \[Str s] _ ->
  putStr s >> return (return $ Str s)

