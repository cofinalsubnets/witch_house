{-# LANGUAGE TupleSections #-}
-- Wisp is a special-purpose Lisp for scripting object behaviour in witch_house.
module WitchHouse.Wisp.Core
( p_eval
, p_apply
, envLookup
, toplevel
, env
, bind
, unbind
, getFrame
, pushFrame
, dropFrame
) where

import WitchHouse.Types
import WitchHouse.Wisp.TC
import Control.Monad

import qualified Data.Map as M
import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString)

import Data.HashTable.IO as H
import Data.Unique
import System.IO.Unsafe


toplevel :: Int
toplevel = hashUnique $ unsafePerformIO newUnique

env :: Env
env = unsafePerformIO $ do
  e <- H.new
  H.insert e toplevel (bindings, Nothing)
  return e
  where
    bindings = M.fromList $
      [ (pack "+",      fold_num (+) )
      , (pack "-",      fold_num (-) )
      , (pack "*",      fold_num (*) )
      , (pack "/",      p_div)
      , (pack "=",      p_eq    )
      , (pack "eval",   p_eval  )
      , (pack "cat",    p_cat   )
      , (pack "apply",  w_apply )
      , (pack "string", p_str)
      , (pack "symbol", p_sym)
      , (pack "bool?", check tc_bool)
      , (pack "string?", check tc_str)
      , (pack "number?", check tc_num)
      , (pack "world?", check tc_world)
      , (pack "func?", check tc_func)
      , (pack "list?", check tc_list)
      , (pack "symbol?", check tc_sym)
      , (pack "primitive?", check tc_prim)
      , (pack "macro?", check tc_macro)
      , (pack "null?",  p_null  )
      , (pack "cons", p_cons    )
      , (pack "error", p_err )
      , (pack "arity", p_arity)
      ]

getFrame :: Int -> IO Frame
getFrame = liftM g . H.lookup env
  where g (Just n) = n
        g Nothing = error "getFrame: missing frame"

dropFrame :: Int -> IO ()
dropFrame = H.delete env

pushFrame :: Frame -> IO Int
pushFrame f = do
  u <- liftM hashUnique newUnique
  H.insert env u f
  return u

bind :: Int -> ByteString -> Sval -> IO ()
bind u k v = do
  (bs,p) <- getFrame u
  H.insert env u (M.insert k v bs, p)

unbind :: Int -> ByteString -> IO ()
unbind u k = do
  (bs,p) <- getFrame u
  H.insert env u (M.delete k bs, p)

{- PRIMITIVES -}

f_quote :: Sval
f_quote = Sform $ \vs _ -> return $ case vs of
  [v] -> Right v
  _ -> len_fail 1 vs

f_quasiq :: Sval
f_quasiq = Sform $ \vs f -> case vs of
  [Slist l] -> spliceL l f
  [sv] -> return $ Right sv
  _ -> return $ len_fail 1 vs
  where
    spliceL l f = do rs <- mapM (splice f) l
                     return $ Slist `fmap` sequence rs
    splice f sv = case sv of
      Slist l@[s, v] -> if s == sym_splice then p_apply p_eval [v] f
                        else spliceL l f
      Slist l -> spliceL l f
      v -> return . return $ v

f_splice :: Sval
f_splice = Sform $ \_ _ -> return $ Left "ERROR: Splice outside of quasiquoted expression."

f_lambda :: Sval
f_lambda = Sform $ \vs f -> return $
  case vs of (Slist ps):svs -> let ps' = map (\(Ssym s) -> s) ps
                               in return $ Sfunc ps' svs f
             _ -> Left "Malformed lambda exp"

f_macro :: Sval
f_macro = Sform $ \vs f -> return $
  case vs of (Slist ps):svs -> let ps' = map (\(Ssym s) -> s) ps
                               in return $ Smacro ps' svs f
             _ -> Left "Malformed macro definition"

f_define :: Sval
f_define = Sform $ \vs f -> case vs of
  [Ssym s, xp] -> do
    xv <- p_apply p_eval [xp] f
    case xv of
      Right v -> bind f s v >> return (Right v)
      Left e -> return $ Left e
  (Slist (h:ss)):xps -> p_apply f_define [h, Slist ([sym_lambda, Slist ss] ++ xps)] f
  _ -> return $ Left "Bad definition syntax"


f_set :: Sval
f_set = Sform $ \vs f -> case vs of
  [Ssym s, xp] -> do
    xv <- p_apply p_eval [xp] f
    case xv of
      Right v -> do
        f' <- findBind s f
        case f' of
          Nothing -> return . Left $ "Free or immutable variable: " ++ show s
          Just n -> bind n s v >> return xv
      Left e -> return $ Left e
  _ -> return $ Left "Bad mutation syntax"

  where findBind nm n
          | n == toplevel = return Nothing
          | otherwise = do (bs, Just n') <- getFrame n
                           if nm `M.member` bs then return (Just n) else findBind nm n'

f_unset :: Sval
f_unset = Sform $ \vs f -> case vs of
  [Ssym s] -> do
    (bs,_) <- getFrame f
    if s `M.member` bs then unbind f s >> (return . Right $ Sbool True)
    else return . Left $ (show s) ++ " is unbound in this context."
  _ -> return $ Left "Bad undefinition syntax"


fold_num :: (Int -> Int -> Int) -> Sval
fold_num op = Sprim $ \vs _ -> return $ do
  tc_fail (and . map tc_num) vs
  return . Snum . foldl1 op $ map (\(Snum n) -> n) vs

p_div :: Sval
p_div = Sprim $ \vs _ -> return $ do
  tc_fail (and . map tc_num) vs
  if any (==Snum 0) (tail vs) then Left "ERROR: divide by zero"
  else return . Snum . foldl1 quot $ map (\(Snum n) -> n) vs

p_err :: Sval
p_err = Sprim $ \err _ -> return $ case err of
  [Sstring e] -> Left ("ERROR: "++e)
  _ -> Left $ "(error) Bad arguments: " ++ show err

p_cat :: Sval
p_cat = Sprim $ \vs _ -> return $ do
  tc_fail (and . map tc_str) vs
  return . Sstring $ concatMap (\(Sstring s) -> s) vs

p_null :: Sval
p_null = Sprim $ \vs _ -> return $ case vs of
  [Slist l] -> return . Sbool $ null l
  [v] -> Left $ "Bad type (expected list): " ++ show v
  _ -> len_fail 1 vs

p_cons :: Sval
p_cons = Sprim $ \vs _ -> return $ case vs of
  [s,Slist l] -> Right $ Slist (s:l)
  [_,l] -> Left $ "Bad type (expected list): " ++ show l
  _ -> len_fail 2 vs

p_eq :: Sval
p_eq = Sprim $ \vs _ -> return . return . Sbool . and . zipWith (==) vs $ drop 1 vs

p_str :: Sval
p_str = Sprim $ \vs _ -> return $ case vs of
  [s@(Sstring _)] -> Right s
  [v] -> Right . Sstring $ show v
  _ -> len_fail 1 vs

p_sym :: Sval
p_sym = Sprim $ \vs _ -> return $ case vs of
  [Sstring s] -> Right . Ssym $ pack s
  [v] -> Left $ "Bad type (expected string):" ++ show v
  _ -> len_fail 1 vs

f_if :: Sval
f_if = Sform $ \vs f -> case vs of
  [cond, y, n] -> do
    v <- p_apply p_eval [cond] f
    case v of Left err -> return $ Left err
              Right v' -> case v' of (Sbool False) -> p_apply p_eval [n] f
                                     _             -> p_apply p_eval [y] f
  _ -> return . Left $ "if: bad conditional syntax: " ++ show (Slist $ sym_if:vs)

f_begin :: Sval
f_begin = Sform $ \sv f -> foldl1 (>>) $ map (\o -> p_apply p_eval [o] f) sv

p_arity :: Sval
p_arity = Sprim $ \sv _ -> case sv of
  [Sfunc as _ _] -> return . Right . Snum . length $ let splat = bs_splat in takeWhile (\s -> s /= splat) as
  [s] -> return . Left $ "Bad type: " ++ show s
  as -> return $ len_fail 1 as

w_apply :: Sval
w_apply = Sprim $ \sv f -> case sv of
  [a,Slist l] -> p_apply a l f
  _ -> return . Left $ "(apply) Bad arguments: " ++ show sv

-- | Function application.
p_apply :: Sval -> [Sval] -> Int -> IO (Either String Sval)
p_apply sv vs i
  | tc_prim sv || tc_form sv = transform sv vs i -- primitive fn application - the easy case!
  | tc_func sv || tc_macro sv = apply posArgs splat vs
  | otherwise = return  . Left $ "Non-applicable value: " ++ show sv
  where
    (posArgs, splat) = break (== bs_splat) (params sv)
    apply pos var sup
      | (not $ null var) && (not $ length var == 2) =
          return . Left $ "Bad variadic parameter syntax: " ++ show (params sv)
      | length pos > length sup || null var && length pos < length sup =
          return $ len_fail (length pos) sup
      | otherwise = let posV = pos `zip` vs
                        varV = if null var then [] else [(last var, Slist $ drop (length pos) vs)]
                    in do
                      n <- pushFrame (M.fromList (varV ++ posV), Just $ frameNo sv)
                      res <- _eval (Slist ((sym_begin):(body sv))) n
                      case res of Left _ -> dropFrame n >> return res
                                  Right r  -> when (not $ tc_macro r || tc_func r) (dropFrame n) >> return res

p_eval :: Sval
p_eval = Sprim $ \v -> case v of
  [v'] -> _eval v'
  _ -> return . return $ len_fail 1 v

_eval :: Sval -> Int -> IO (Either String Sval)
_eval v f
  | tc_sym  v = _eval_var v
  | special v = _apply_spec v
  | tc_list v = _apply v
  | otherwise = return $ return v

  where
    special (Slist (Ssym s:_)) = s `M.member` specialForms
    special _ = False

    _apply_spec (Slist ((Ssym s):t)) = let Sform form = specialForms M.! s in form t f
    _apply_spec _ = error "_eval: _apply_spec: unexpected pattern"

    _eval_var (Ssym sv) = envLookup sv $ Just f
    _eval_var _ = error "_eval: _eval_var: unexpected pattern"

    _apply (Slist []) = return $ return (Slist [])
    _apply (Slist (o:vs)) = do
      op <- _eval o f
      case op of
        Left err -> return $ Left err
        Right op' -> if not $ tc_macro op' then do
                       vals <- evalList vs f
                       case vals of
                         Right vals' -> p_apply op' vals' f
                         Left err -> return $ Left err
                     else do expn <- p_apply op' vs f
                             case expn of Left err -> return $ Left err
                                          Right xv -> _eval xv f

    _apply _ = error "_eval: _apply: unexpected pattern"


envLookup :: ByteString -> Maybe Int -> IO (Either String Sval)
envLookup s Nothing = return . Left $ "Unable to resolve symbol: " ++ show s
envLookup s (Just i) = do
  (binds,nxt) <- getFrame i
  case M.lookup s binds of Nothing -> envLookup s nxt
                           Just v -> return $ Right v

{-
-- | VERY primitive reference-counting garbage collection.
gc :: Env -> Env
gc env = foldl (flip M.delete) env (M.keys env \\ ks)
  where ks = [0] ++ (map frameNo . filter tc_func $ concatMap M.elems (map fst $ M.elems env))
-}

specialForms :: M.Map ByteString Sval
specialForms = M.fromList $
  [ (pack "define", f_define)
  , (pack "begin",  f_begin )
  , (pack "quote",  f_quote )
  , (pack "if",     f_if    )
  , (pack "lambda", f_lambda)
  , (pack "set!",   f_set   )
  , (pack "quasiquote", f_quasiq)
  , (pack "splice", f_splice)
  , (pack "macro", f_macro)
  , (pack "unset!", f_unset)
  ]

{- helpers -}

evalList :: [Sval] -> Int -> IO (Either String [Sval])
evalList vs f = liftM sequence $ mapM (\o -> p_apply p_eval [o] f) vs

check :: (Sval -> Bool) -> Sval
check p = Sprim $ \vs f -> do
  vs' <- evalList vs f
  case vs' of 
    Right vs'' -> return . return . Sbool $ all p vs''
    Left err -> return $ Left err

{- symbol & bytestring constants -}
sym_if :: Sval
sym_if = Ssym $ pack "if"
sym_begin :: Sval
sym_begin = Ssym $ pack "begin"
sym_lambda :: Sval
sym_lambda = Ssym $ pack "lambda"
sym_splice :: Sval
sym_splice = Ssym $ pack "splice"
bs_splat :: ByteString
bs_splat = pack "."

