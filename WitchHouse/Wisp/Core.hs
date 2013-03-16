{-# LANGUAGE TupleSections #-}
-- Wisp is a special-purpose Lisp for scripting object behaviour in witch_house.
module WitchHouse.Wisp.Core
( coreBinds
, evalList
, p_eval
, p_apply
, gc
, envLookup
) where

import WitchHouse.Types
import Control.Monad

import qualified Data.Map as M
import Data.List


{- PRIMITIVES -}

f_quote :: Sval
f_quote = Sform $ \vs _ e -> return $
  case vs of [v] -> (Right v,e)
             _ -> (Left $ "Wrong number of arguments: " ++ show (length vs) ++ " for 1", e)

f_lambda :: Sval
f_lambda = Sform $ \vs f env -> return $
  case vs of (Slist ps):svs -> let ps' = map (\(Ssym s) -> s) ps
                               in (return $ Sfunc ps' svs f, env)
             _ -> (Left "Malformed lambda exp", env)

f_define :: Sval
f_define = Sform $ \vs f env -> case vs of
  [Ssym s, xp] -> do
    (xv,env') <- run (p_apply p_eval [xp] f) env
    case xv of
      Right v -> let (frame, p) = env M.! f
                     frame' = M.insert s v frame
                     env'' = M.insert f (frame',p) env'
                 in return (Right v, env'')
      Left e -> return (Left e, env)
  (Slist (h:ss)):xps -> run (p_apply f_define [h, Slist ([Ssym "lambda", Slist ss] ++ xps)] f) env
  _ -> return (Left "Bad definition syntax", env)


f_set :: Sval
f_set = Sform $ \vs f env -> case vs of
  [Ssym s, xp] -> do
    (xv,env') <- run (p_apply p_eval [xp] f) env
    return $ case xv of
      Right v -> case findBind s f env' of
                   Nothing -> (Left $ "Unable to resolve symbol: " ++ s, env)
                   Just n -> let (fr,n') = env M.! n
                             in (Right v, M.insert n (M.insert s v fr, n') env')
      Left e -> (Left e, env)
  _ -> return (Left "Bad definition syntax", env)

  where findBind _ (-1) _ = Nothing
        findBind nm n e = let (bs,n') = e M.! n in if nm `M.member` bs then Just n else findBind nm n' e

fold_num :: (Int -> Int -> Int) -> Sval
fold_num op = Sprim $ \vs _ env ->
  let ret = do tc_fail (and . map tc_num) vs
               return . Snum . foldl1 op $ map (\(Snum n) -> n) vs
  in return (ret,env)

p_err :: Sval
p_err = Sprim $ \err _ env -> return $ case err of
  [Sstring e] -> (Left ("ERROR: "++e), env)
  _ -> (Left $ "(error) Bad arguments: " ++ show err, env)
  

p_cat :: Sval
p_cat = Sprim $ \vs _ e ->
  let ret = do tc_fail (and . map tc_str) vs
               return . Sstring $ concatMap (\(Sstring s) -> s) vs
  in return (ret, e)

p_null :: Sval
p_null = Sprim $ \vs _ e -> return $ case vs of
  [Slist []] -> (return $ Sbool True, e)
  [Slist _] -> (return $ Sbool False, e)
  [v] -> (Left $ "Bad argument type: " ++ show v, e)
  _ -> (Left $ "Wrong number of arguments: " ++ show (length vs) ++ " for 1", e)

p_cons :: Sval
p_cons = Sprim $ \vs _ e -> return $ case vs of
  [s,Slist l] -> (Right (Slist (s:l)), e)
  [_,l] -> (Left $ "Bad type (expected list): " ++ show l, e)
  _ -> (Left $ "Wrong number of arguments: " ++ show (length vs) ++ " for 2", e)

p_eq :: Sval
p_eq = Sprim $ \vs _ e -> return (return $ Sbool . and . zipWith (==) vs $ drop 1 vs, e)

f_if :: Sval
f_if = Sform $ \vs f env -> case vs of
  [cond, y, n] -> do
    (v,_) <- run (p_apply p_eval [cond] f) env
    case v of Left err -> return (Left err, env)
              Right v' -> case v' of (Sbool False) -> run (p_apply p_eval [n] f) env
                                     _             -> run (p_apply p_eval [y] f) env
  _ -> return (Left $ "if: bad conditional syntax: " ++ show (Slist $ (Ssym "if"):vs), env)

f_begin :: Sval
f_begin = Sform $ \sv f env -> run (foldl1 (>>) $ map (\o -> p_apply p_eval [o] f) sv) env

w_apply :: Sval
w_apply = Sprim $ \sv f env -> case sv of
  [a,Slist l] -> run (p_apply a l f) env
  _ -> return (Left $ "Bad arguments: " ++ show sv, env)

-- | Function application.
p_apply :: Sval -> [Sval] -> Int -> Expr (Either String Sval)

p_apply (Sprim f) vs i = Expr $ f vs i -- primitive fn application - the easy case!
p_apply (Sform f) vs i = Expr $ f vs i

p_apply (Sfunc ps body fe) vs _ = Expr $ apply posArgs splat vs
  where
    (posArgs, splat) = break (== ".") ps
    apply pos var sup env
      | (not $ null var) && (not $ length var == 2) =
          return (Left $ "Bad variadic parameter syntax: " ++ show ps, env)
      | length pos > length sup || null var && length pos < length sup =
          return (Left $ "(apply) Wrong number of arguments: " ++ show (length sup) ++ " for " ++ show (length pos), env)
      | otherwise = let posV = pos `zip` vs
                        varV = if null var then [] else [(last var, Slist $ drop (length pos) vs)]
                        frame = (M.fromList (varV ++ posV), fe)
                        (n,env') = pushFrame frame env
                    in _eval [Slist ((Ssym "begin"):body)] n env'

    pushFrame f e = let n = succ (last $ M.keys e) in (n, M.insert n f e)

p_apply v _ _ = Expr $ \env -> return (Left $ "Non-applicable value: " ++ show v, env)


p_eval :: Sval
p_eval = Sprim _eval

_eval :: [Sval] -> Int -> Env -> IO (Either String Sval, Env)
_eval [v] f env
  | prim v    = return (return v, env)
  | spec v    = _apply_spec v
  | tc_sym v  = _eval_var v
  | tc_list v = _apply v
  | otherwise = return (Left $ "Bad argument type: " ++ show v, env)

  where

    prim sv = or $ map ($sv) [tc_str, tc_bool, tc_num, tc_func, tc_prim, tc_world, tc_form]
    spec sv = case sv of Slist (Ssym s:_) -> s `M.member` specialForms
                         _ -> False

    _apply_spec (Slist ((Ssym s):t)) = let Sform form = specialForms M.! s in form t f env
    _apply_spec _ = error "_eval: _apply_spec: unexpected pattern"

    _eval_var (Ssym sv) = return $ case envLookup sv f env of
                            Just val -> (return val, env)
                            Nothing -> (Left $ "Unable to resolve symbol: " ++ sv, env)
    _eval_var _ = error "_eval: _eval_var: unexpected pattern"

    _apply (Slist (o:vs)) = do
      (op, _) <- _eval [o] f env
      vals <- mapM (\val -> fst `fmap` _eval [val] f env) vs

      case (op,sequence vals) of
        (Right op',Right vals') -> run (p_apply op' vals' f) env
        (Left err,_) -> return (Left err,env)
        (_,Left err) -> return (Left err,env)

    _apply nil@(Slist []) = return (return nil, env)
    _apply _ = error "_eval: _apply: unexpected pattern"

_eval a _ e = return (Left $ "eval: wrong number of arguments: " ++ show (length a) ++ " for 1", e)


envLookup :: String -> Int -> Env -> Maybe Sval
envLookup _ (-1) _ = Nothing 
envLookup s f env = let (binds,nxt) = env M.! f
  in case M.lookup s binds of Nothing -> envLookup s nxt env
                              Just v -> Just v

evalList :: [Sval] -> Int -> Env -> IO (Either String [Sval])
evalList vs f env = do
  let acc (l,s) m = run m s >>= \(r,s') -> return (r:l,s')
  (vs',_) <- foldM acc ([],env) $ map (\o -> p_apply p_eval [o] f) vs
  return $ sequence (reverse vs')

-- | VERY primitive reference-counting garbage collection.
gc :: Env -> Env
gc env = foldl (flip M.delete) env (M.keys env \\ nub ks)
  where ks = [0] ++ (map frame . filter func $ concatMap M.elems (map fst $ M.elems env))
        func sv = case sv of { Sfunc _ _ _ -> True; _ -> False }
        frame sv = case sv of { Sfunc _ _ n -> n; _ -> error $ "gc: bad Sval type: " ++ show sv }

specialForms :: M.Map String Sval
specialForms = M.fromList $
  [ ("define", f_define)
  , ("begin",  f_begin )
  , ("quote",  f_quote )
  , ("if",     f_if    )
  , ("lambda", f_lambda)
  , ("set!",   f_set   )
  ]

coreBinds :: M.Map String Sval
coreBinds = M.fromList $
  [ ("+",      fold_num (+) )
  , ("-",      fold_num (-) )
  , ("*",      fold_num (*) )
  , ("/",      fold_num quot)
  , ("=",      p_eq    )
  , ("eval",   p_eval  )
  , ("cat",    p_cat   )
  , ("apply",  w_apply )
  , ("bool?", check tc_bool)
  , ("string?", check tc_str)
  , ("number?", check tc_num)
  , ("world?", check tc_world)
  , ("func?", check tc_func)
  , ("list?", check tc_list)
  , ("symbol?", check tc_sym)
  , ("primitive?", check tc_prim)
  , ("null?",  p_null  )
  , ("cons", p_cons    )
  , ("error", p_err )
  ]

{- TYPE PREDICATES -}

tc_str :: Sval -> Bool
tc_str s = case s of { Sstring _ -> True; _ -> False }
tc_num :: Sval -> Bool
tc_num s = case s of { Snum _ -> True; _ -> False }
tc_bool :: Sval -> Bool
tc_bool s = case s of { Sbool _ -> True; _ -> False }
tc_func :: Sval -> Bool
tc_func s = case s of { Sfunc _ _ _ -> True; _ -> False }
tc_world :: Sval -> Bool
tc_world s = case s of { Sworld _ -> True; _ -> False }


tc_sym :: Sval -> Bool
tc_sym s = case s of { Ssym _ -> True; _ -> False }
tc_list :: Sval -> Bool
tc_list s = case s of { Slist _ -> True; _ -> False }
tc_prim :: Sval -> Bool
tc_prim s = case s of { Sprim _ -> True; _ -> False }
tc_form :: Sval -> Bool
tc_form s = case s of { Sform _ -> True; _ -> False }


tc_fail :: Show a => (a -> Bool) -> a -> Either String ()
tc_fail p v = if p v then return () else Left $ "Bad type: " ++ show v

check :: (Sval -> Bool) -> Sval
check p = Sprim $ \vs f e -> do
  vs' <- evalList vs f e 
  case vs' of 
    Right vs'' -> return (return . Sbool $ all p vs'', e)
    Left err -> return (Left err,e)

