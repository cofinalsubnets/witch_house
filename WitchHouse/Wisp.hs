module WitchHouse.Wisp
( toplevel
, runWisp
, repl
) where

import WitchHouse.Types
import WitchHouse.Wisp.Core
import WitchHouse.Wisp.Parser

import Data.Map (fromList)

import System.IO -- for the repl
import System.IO.Unsafe (unsafePerformIO) -- for toplevel defs

toplevel :: Env
toplevel = snd . unsafePerformIO $ run defs base
  where 
    base = fromList [(0, (coreBinds, -1))]
    defs = runWisp . unlines $
      [ "(begin"

      , "  (define (list . l) l)"

      , "  (define (member e lst)"
      , "    (if (null? lst)"
      , "        #f"
      , "        (if (= (car lst) e)"
      , "            lst"
      , "            (member e (cdr lst)))))"

      , "  (define (length l)"
      , "    (if (null? l)"
      , "        0"
      , "        (+ 1 (length (cdr l)))))"
      , "  (define (comp f g)"
      , "    (lambda (n) (f (g n))))"

      , "  (define (car l)"
      , "    (if (null? l)"
      , "        (error \"car: null list\")"
      , "        (apply (lambda (h . t) h)"
      , "               l)))"

      , "  (define (cdr l)"
      , "    (if (null? l)"
      , "        l"
      , "        (apply (lambda (h . t) t)"
      , "               l)))"

      , "  (define caar (comp car car))"
      , "  (define cadr (comp car cdr))"
      , "  (define cddr (comp cdr cdr))"
      , "  (define cdar (comp cdr car))"

      , "  (define caaar (comp car caar))"
      , "  (define caadr (comp car cadr))"
      , "  (define cadar (comp car cdar))"
      , "  (define caddr (comp car cddr))"
      , "  (define cdaar (comp cdr caar))"
      , "  (define cdadr (comp cdr cadr))"
      , "  (define cddar (comp cdr cdar))"
      , "  (define cdddr (comp cdr cddr))"

      , "  (define (assoc v l)"
      , "    (if (null? l)"
      , "        #f"
      , "        (if (= v (caar l))"
      , "            (car (cdar l))"
      , "            (assoc v (cdr l)))))"

      , "  (define (map op l)"
      , "    (if (null? l)"
      , "        l"
      , "        (cons (op (car l))"
      , "              (map op (cdr l)))))"

      , "  (define (filter p l)"
      , "    (if (null? l)"
      , "        l"
      , "        (if (p (car l))"
      , "            (cons (car l)"
      , "                  (filter p (cdr l)))"
      , "            (filter p (cdr l)))))"

      , "  (define (fold op acc l)"
      , "    (if (null? l)"
      , "        acc"
      , "        (fold op (op acc (car l)) (cdr l))))"

      , "  (define (not v) (if v #f #t))"
      , "  (define (and a b) (if a b a))"
      , "  (define (or a b) (if a a b))"

      , "  (define (reverse l)"
      , "    (define (inner acc l)"
      , "      (if (null? l)"
      , "          acc"
      , "          (inner (cons (car l) acc)"
      , "                 (cdr l))))"
      , "    (inner '() l))"

      , "  (define (inc n) (+ n 1))"
      , "  (define (dec n) (- n 1))"
      , "  (define (id n) n)"
      , "  (define (const x) (lambda (y) x))"

      , "  (define (remainder a b)"
      , "    (- a (* b (/ a b))))"
      , "  (define (even? n)"
      , "    (= 0 (remainder n 2)))"
      , "  (define odd? (comp not even?))"
      
      , "  (define (append l1 l2)"
      , "    (if (null? l1)"
      , "        l2"
      , "        (cons (car l1)"
      , "              (append (cdr l1) l2))))"

      , "  (define (juxt f g)"
      , "    (lambda (h . t)"
      , "      (list (apply f (cons h t))"
      , "            (apply g (cons h t)))))"

      , ")"
      ]

repl :: IO ()
repl = loop toplevel
  where loop bs = do putStr "\n> "
                     hFlush stdout
                     l <- getLine
                     case l of
                       "\\env" -> putStr (show bs) >> loop bs
                       "\\quit" -> return ()
                       "" -> loop bs
                       _ -> do res <- run (runWisp l) bs
                               case res of
                                 (Left err, bs') -> putStr err >> loop bs'
                                 (Right v, bs') -> putStr (show v) >> loop bs'

runWisp :: String -> Expr (Either String) Sval
runWisp s = Expr $ \e -> case parseWisp s of
  Right sv -> do (v,e') <- run (p_apply p_eval [sv] 0) e
                 return (v, gc e')
  Left err -> return (Left $ show err, e)

