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
    defs = mapM runWisp $
      [ "(define (list . l) l)"
      , "(define (member e lst) (if (null? lst) #f (if (= (car lst) e) lst (member e (cdr lst)))))"
      , "(define (length l) (if (null? l) 0 (+ 1 (length (cdr l)))))"
      , "(define (caar l) (car (car l)))"
      , "(define (cadr l) (car (cdr l)))"
      , "(define (cddr l) (cdr (cdr l)))"
      , "(define (cdar l) (cdr (car l)))"
      , "(define (assoc v l) (if (null? l) #f (if (= v (caar l)) (car (cdar l)) (assoc v (cdr l)))))"
      , "(define (map op l) (if (null? l) l (cons (op (car l)) (map op (cdr l)))))"
      , "(define (filter p l) (if (null? l) l (if (p (car l)) (cons (car l) (filter p (cdr l))) (filter p (cdr l)))))"
      , "(define (not v) (if v #f #t))"
      , "(define (comp f g) (lambda (n) (f (g n))))"
      , "(define (reverse l) (define (inner acc l) (if (null? l) acc (inner (cons (car l) acc) (cdr l)))) (inner '() l))"
      ]

invoke :: String -> [Sval] -> Env -> Either String Sval
invoke f sv e = case envLookup f 0 e of
  Nothing -> Left $ "Unable to resolve symbol: " ++ f
  Just fn -> fst $ run (prim_apply fn sv 0) e

runWisp :: String -> Expr (Either String Sval)
runWisp s = Expr $ \e -> case parseWisp s of Right sv -> let (v,e') = run (prim_eval sv 0) e in (v, gc e')
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

