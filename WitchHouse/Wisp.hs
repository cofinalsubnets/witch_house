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
toplevel = fromList [(0, (binds, -1))]
  where binds = coreBinds `union` worldOps

invoke :: String -> [Sval] -> Env -> Either String Sval
invoke f sv e = case envLookup f 0 e of
  Nothing -> Left $ "Unable to resolve symbol: " ++ f
  Just fn -> fst $ run (prim_apply fn sv 0) e

runWisp :: String -> Expr (Either String Sval)
runWisp s = Expr $ \e -> case parseWisp s of Right sv -> let (v,e') = run (prim_eval sv 0) e in (v, gc e')
                                             Left err -> (Left $ show err, e)

evalWisp :: String -> WT
evalWisp s (o,cs) = let o' = bindAttrs o
  in case run (runWisp s) (bindings o') of
       (Right _, env) -> Right (applyAttrs o'{bindings = env},cs)
       (Left err, _) -> Left err

bindAttrs :: Obj -> Obj
bindAttrs o = let bs = fromList [ ("*name*", Sstring $ name o)
                                , ("*desc*", Sstring $ description o)
                                ]
                  (tl,_) = bindings o ! 0
  in o{bindings = insert 0 ((tl `union` bs),-1) (bindings o)}

applyAttrs :: Obj -> Obj
applyAttrs o = apName . apDesc $ o
  where (tl,_) = bindings o ! 0
        apName obj = case M.lookup "*name*" tl of { Just (Sstring n) -> obj{name = n} ; _ -> obj }
        apDesc obj = case M.lookup "*desc*" tl of { Just (Sstring d) -> obj{description = d}; _ -> obj }

