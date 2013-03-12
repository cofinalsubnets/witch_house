module WitchHouse.Wisp
( runWisp
, toplevelBindings
) where

import Data.Maybe
import qualified Data.Map as M
import Data.Monoid

import WitchHouse.Wisp.Types
import WitchHouse.Wisp.Parser

runWisp :: String -> Env -> (Either String Sval, Env)
runWisp s e = case parseWisp s of Right sv -> eval' sv e
                                  Left _ -> (Left "Parse error", e)
  where eval' vs env = let (r,env') = run (prim_eval vs) env
                       in case r of Right l@(Slist _) -> eval' l env'
                                    _ -> (r, env')

{- PRIMITIVES -}

prim_lambda :: Sval
prim_lambda = Sprim $ \vs env ->
  case vs of (Slist ps):svs -> let ps' = map (\(Ssym s) -> s) ps
                               in (return $ Sfunc ps' svs env, env)
             _ -> (Left "Malformed lambda exp", env)

prim_define :: Sval
prim_define = Sprim $ \vs env@(f:fs) ->
  case vs of [Ssym s, xp] -> let xv = fst $ run (prim_eval xp) env
                             in case xv of Right v -> (Right v, (M.insert s v f):fs)
                                           Left e -> (Left e, f:fs)
             _ -> (Left "Bad definition syntax", env)

fold_num :: (Int -> Int -> Int) -> Sval
fold_num op = Sprim $ \vs env ->
  let (vs',env') = evalList vs env
      ret = do vals <- vs'
               case fst $ run (prim_apply nump vals) env of
                 Right (Sbool True) -> return . foldl1 op $ map (\(Snum n) -> n) vals
                 Right _ -> Left $ "Bad type (expected numeric)"
                 Left err -> Left err
  in (fmap Snum ret, env')

prim_add :: Sval
prim_add = fold_num (+)
prim_sub :: Sval
prim_sub = fold_num (-)
prim_mul :: Sval
prim_mul = fold_num (*)
prim_div :: Sval
prim_div = fold_num quot

prim_if :: Sval
prim_if = Sprim $ \vs env ->
  case vs of
    [cond, y, n] -> let (v,_) = run (prim_eval cond) env
      in case v of Left err -> (Left err, env)
                   Right v' -> case v' of (Sbool False) -> run (prim_eval n) env
                                          _             -> run (prim_eval y) env


-- | Function application.
-- Handles fns defined in wisp, primitive fns, and exprs that (may) evaluate
-- to fns as separate cases.
-- TODO: see if this can be shorter & add support for special forms!
prim_apply :: Sval -> [Sval] -> Expr (Either String Sval)
prim_apply (Sfunc ps body fe) vs = Expr $ \env ->
  if length ps /= length vs then (Left $ "Wrong number of arguments: " ++ show (length vs) ++ " for " ++ show (length ps), env)
    else let (vs', env') = evalList vs env
             ret = do vals <- vs'
                      let fe' = (M.fromList (ps `zip` vals)):fe -- push a new frame onto the stack
                      fst $ run (foldl1 (>>) $ map prim_eval body) fe'
         in (ret,env')
prim_apply (Sprim f) vs = Expr $ \env -> f vs env

prim_apply (Ssym s) vs = Expr $ \env ->
  case envLookup s env of
    Nothing -> (Left $ "Unable to resolve symbol: " ++ s, env)
    Just v  -> if v == Ssym s then (Left $ "Circular definition: " ++ s, env)
                              else run (prim_apply v vs) env

prim_apply (Sform ps body) vs = Expr $ \env ->
  if length ps /= length vs then (Left $ "Wrong number of arguments: " ++ show (length vs) ++ " for " ++ show (length ps), env)
    else let env' = (M.fromList (ps `zip` vs)):env
             ret = fst $ run (foldl1 (>>) $ map prim_eval body) env'
         in (ret,env')


prim_apply (Slist l) vs = Expr $ \env -> 
  let fn = fst $ run (prim_eval $ Slist l) env
      ret = do fn' <- fn
               return $ run (prim_apply fn' vs) env
  in case ret of Right v -> v
                 Left err -> (Left err, env)
prim_apply v _ = Expr $ \env -> (Left $ "Non-applicable value: " ++ show v, env)

-- | Sval evaluation.
prim_eval :: Sval -> Expr (Either String Sval)
prim_eval sv = Expr $ \env ->
  case sv of Slist (o:vs) -> run (prim_apply o vs) env
             Ssym s -> case envLookup s env of
                         Just v -> (return v, env)
                         Nothing -> (Left $ "Unable to resolve sumbol: " ++ s, env)
             _ -> (return sv, env)

envLookup :: String -> Env -> Maybe Sval
envLookup _ [] = Nothing
envLookup s (f:fs) = case M.lookup s f of Nothing -> envLookup s fs
                                          Just v -> Just v

evalList :: [Sval] -> Env -> (Either String [Sval], Env)
evalList vs env = let (vs',env') = foldl acc ([],env) $ map prim_eval vs
                      acc (l,s) m = let (r,s') = run m s in (r:l,s')
                  in (sequence vs', env')


toplevelBindings = [frame]
  where frame = M.fromList $
          [ ("lambda", prim_lambda)
          , ("+",      prim_add   )
          , ("-",      prim_sub   )
          , ("*",      prim_mul   )
          , ("/",      prim_div   )
          , ("def",    prim_define)
          , ("if",     prim_if    )
          ]

{- TYPE PREDICATES -}

-- FIXME: these don't eval their arguments properly

stringp :: Sval
stringp = Sprim $ \vs e -> (return . Sbool $ all str vs, e)
  where str v = case v of Sstring _ -> True
                          _ -> False

nump :: Sval
nump = Sprim $ \vs e -> (return . Sbool $ all num vs, e)
  where num v = case v of Snum _ -> True
                          _ -> False

boolp :: Sval
boolp = Sprim $ \vs e -> (return . Sbool $ all bln vs, e)
  where bln v = case v of Sbool _ -> True
                          _ -> False

funcp :: Sval
funcp = Sprim $ \vs e -> (return . Sbool $ all fn vs, e)
  where fn v = case v of Sfunc _ _ _ -> True
                         _ -> False

