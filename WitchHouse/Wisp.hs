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

import Data.Map (fromList, union)

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
evalWisp s (o@(Obj {bindings = b}),cs) = case run (runWisp s) b of
  (Right _, env) -> Right (o{bindings = env},cs)
  (Left err, _) -> Left err

