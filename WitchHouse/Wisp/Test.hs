module WitchHouse.Wisp.Test (testLib) where

import WitchHouse.Types
import WitchHouse.Wisp.Predicates
import Data.ByteString.Char8 (pack)

testLib :: Module
testLib = (lib,defs)
  where
    lib = [ (pack "print", t_print) ]
    defs = unlines $
      [ "(begin"
      , "  (define (colorize color s)"
      , "    (cat color s \"\x1b[0m\"))"

      , "  (define red  \"\x1b[31m\")"
      , "  (define green  \"\x1b[32m\")"
      , "  (define (println s) (print (cat s \"\\n\")))"
      , "  (define test (macro (suite & ts)"
      , "    (if (list? suite)"
      , "        (set! ts (cons suite ts))"
      , "        (println (cat \"Running \" suite \" ...\")))"
      , "    (define (pass) (print (colorize green \".\")))"
      , "    (define (fail) (print (colorize red \"F\")))"
      , "    (define failures '())"
      , "    (map (lambda (x)"
      , "           (let ((doc (car x)) (t (cadr x)))"
      , "             (if (eval t)"
      , "                 (pass)"
      , "                 (begin (set! failures (cons doc failures))"
      , "                        (fail)))))"
      , "         ts)"
      , "    (println \"\")"
      , "    (if (null? failures)"
      , "      (begin (println \"ok!\") #t)"
      , "      (begin"
      , "        (println (cat (string (length failures)) \" failure(s): \"))"
      , "        (map println failures)"
      , "        #f))))"
      , ")"
      ]

t_print = Sprim $ lc 1 $ tc [tc_str] $ \[Sstring s] _ ->
  putStr s >> return (return $ Sstring s)

