module WitchHouse.Wisp
( toplevel
, invoke
, runWisp
, evalWisp
) where

import WitchHouse.Types
import WitchHouse.Wisp.Core
import WitchHouse.Wisp.Parser
import WitchHouse.Wisp.WorldOp

import Data.Map (fromList, union, (!), insert)
import qualified Data.Map as M

toplevel :: Env
toplevel = snd $ run defs base
  where 
    base = fromList [(0, (binds, -1))]
    binds = coreBinds `union` worldOps
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

      , "  (define (reduce op acc l)"
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

      , "  (define (juxt f g)"
      , "    (lambda (h . t)"
      , "      (list (apply f (cons h t))"
      , "            (apply g (cons h t))))))"
      ]

invoke :: String -> [Sval] -> World -> Either String (Sval,World)
invoke f sv w = let (o,cs) = bindAttrs w
  in case envLookup f 0 (bindings o) of
       Nothing -> Left $ "Unable to resolve symbol: " ++ f
       Just fn -> case run (p_apply fn sv 0) (bindings o) of
                    (Right s,env) -> Right (s, (applyAttrs o{bindings = env},cs))
                    (Left err,_)  -> Left err

runWisp :: String -> Expr (Either String Sval)
runWisp s = Expr $ \e -> case parseWisp s of
  Right sv -> let (v,e') = run (p_apply p_eval [sv] 0) e in (v, gc e')
  Left err -> (Left $ show err, e)

evalWisp :: String -> WT
evalWisp s w = let (o,cs) = bindAttrs w
  in case run (runWisp s) (bindings o) of
       (Right (Sworld w'), _) -> Right w'
       (Right _, env) -> Right (applyAttrs o{bindings = env},cs)
       (Left err, _) -> Left err

bindAttrs :: World -> World
bindAttrs (o,cs) = let bs = fromList [ ("*name*", Sstring $ name o)
                                     , ("*desc*", Sstring $ description o)
                                     , ("*world*", Sworld (o,cs))
                                     ]
                       (tl,_) = bindings o ! 0
  in (o{bindings = insert 0 ((tl `union` bs),-1) (bindings o)},cs)

applyAttrs :: Obj -> Obj
applyAttrs o = apName . apDesc $ o
  where (tl,_) = bindings o ! 0
        apName obj = case M.lookup "*name*" tl of { Just (Sstring n) -> obj{name = n} ; _ -> obj }
        apDesc obj = case M.lookup "*desc*" tl of { Just (Sstring d) -> obj{description = d}; _ -> obj }

