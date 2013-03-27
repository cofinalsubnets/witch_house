module WitchHouse.Wisp.Test (loadTestLib) where

import WitchHouse.Wisp
import WitchHouse.Types
import WitchHouse.Wisp.Predicates
import WitchHouse.Wisp.Core (apply)
import Data.ByteString.Char8 (pack)

loadTestLib = do
  mapM_ (\(s,f) -> bind toplevel s f) lib
  evalWisp defs toplevel
  
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
      , "    (map println failures)"
      , "    (null? failures)))"
      , ")"
      ]

t_print = Sprim $ lc 1 $ tc [tc_str] $ \[Sstring s] _ ->
  putStr s >> return (return $ Sstring s)

