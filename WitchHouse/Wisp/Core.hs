{-# LANGUAGE TupleSections, RankNTypes #-}
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
, gc
, gcW
) where

import WitchHouse.Types
import Control.Monad
import Data.List
import Data.Graph
import qualified Data.Map as M
import qualified Data.HashTable.IO as H
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString (ByteString)
import System.IO.Unsafe
import System.Random

toplevel :: Int
toplevel = 0

-- what's a functional programming language without global mutable state?
env :: H.BasicHashTable Int Frame
env = unsafePerformIO $ do
  e <- H.new
  H.insert e toplevel (bindings, Nothing)
  return e
  where
    bindings = M.fromList $
      [ (pack "+",          fold_num (+)   )
      , (pack "-",          fold_num (-)   )
      , (pack "*",          fold_num (*)   )
      , (pack "/",          p_div          )
      , (pack "=",          p_eq           )
      , (pack "eval",       p_eval         )
      , (pack "cat",        p_cat          )
      , (pack "apply",      w_apply        )
      , (pack "string",     p_str          )
      , (pack "symbol",     p_sym          )
      , (pack "null?",      p_null         )
      , (pack "cons",       p_cons         )
      , (pack "error",      p_err          )
      , (pack "arity",      p_arity        )
      , (pack "bool?",      check tc_bool  )
      , (pack "string?",    check tc_str   )
      , (pack "number?",    check tc_num   )
      , (pack "world?",     check tc_world )
      , (pack "func?",      check tc_func  )
      , (pack "list?",      check tc_list  )
      , (pack "symbol?",    check tc_sym   )
      , (pack "primitive?", check tc_prim  )
      , (pack "macro?",     check tc_macro )
      , (pack "handle?",    check tc_handle)
      , (pack "ref?",       check tc_ref   )
      , (pack "make-ref",   p_mk_ref       )
      , (pack "make-self-ref", p_mk_self_ref)
      ]

    fold_num :: (forall a. Num a => a -> a -> a) -> Sval
    fold_num op = Sprim $ \vs _ -> return $ do
      tc_fail (and . map tc_num) vs
      tc_fail (not . null) vs
      foldM (s_num_op op) (head vs) (tail vs)

    check p = Sprim $ \vs f -> do
      vs' <- evalList vs f
      case vs' of 
        Right vs'' -> return . return . Sbool $ all p vs''
        Left err -> return $ Left err


getFrame :: Int -> IO Frame
getFrame = liftM g . H.lookup env
  where g (Just n) = n
        g Nothing = error "getFrame: missing frame"

dropFrame :: Int -> IO ()
dropFrame = H.delete env

pushFrame :: Frame -> IO Int
pushFrame f = do
  u <- randomIO
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

envLookup :: ByteString -> Maybe Int -> IO (Either String Sval)
envLookup s Nothing = return . Left $ "Unable to resolve symbol: " ++ unpack s
envLookup s (Just i) = do
  (binds,nxt) <- getFrame i
  case M.lookup s binds of Nothing -> envLookup s nxt
                           Just v -> return $ Right v


-- Primitive fns and special forms

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
      Slist l@[s, v] -> if s == sym_splice then _eval v f
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
             _ -> Left "Malformed macro"

f_define :: Sval
f_define = Sform $ \vs f -> case vs of
  [Ssym s, xp] -> do
    xv <- _eval xp f
    case xv of
      Right v -> bind f s v >> return (Right v)
      Left e -> return $ Left e
  (Slist (h:ss)):xps -> p_apply f_define [h, Slist ([sym_lambda, Slist ss] ++ xps)] f
  _ -> return $ Left "Bad definition syntax"


f_set :: Sval
f_set = Sform $ \vs f -> case vs of
  [Ssym s, xp] -> do
    xv <- _eval xp f
    case xv of
      Right v -> do
        f' <- findBinding s f
        case f' of
          Nothing -> return . Left $ "Free or immutable variable: " ++ unpack s
          Just n -> bind n s v >> return xv
      Left e -> return $ Left e
  _ -> return $ Left "Bad mutation syntax"

f_unset :: Sval
f_unset = Sform $ \vs f -> case vs of
  [Ssym s] -> do
    f' <- findBinding s f
    case f' of
      Nothing -> return . Left $ "Free or immutable variable: " ++ unpack s
      Just n -> do (b,_) <- getFrame n
                   let v = b M.! s
                   unbind n s
                   return $ Right v
  _ -> return $ Left "Bad undefinition syntax"

findBinding :: ByteString -> Int -> IO (Maybe Int)
findBinding nm n
  | n == toplevel = return Nothing
  | otherwise = do (bs, Just n') <- getFrame n
                   if nm `M.member` bs then return (Just n) else findBinding nm n'

s_num_op :: (forall a. Num a => a -> a -> a) -> Sval -> Sval -> Either String Sval
s_num_op (?) s1 s2 = case (s1,s2) of
  (Sfixn a, Sfixn b)   -> Right . Sfixn  $ a ? b
  (Sfixn a, Sfloat b)  -> Right . Sfloat $ fromIntegral a ? b
  (Sfloat a, Sfixn b)  -> Right . Sfloat $ a ? fromIntegral b
  (Sfloat a, Sfloat b) -> Right . Sfloat $ a ? b
  _ -> Left $ "Bad types: " ++ show (Slist [s1,s2])

s_div :: Sval -> Sval -> Either String Sval
s_div s1 s2 = case (s1,s2) of
  (Sfixn _,  Sfixn 0) -> Left "Divide by zero"
  (Sfloat _, Sfixn 0) -> Left "Divide by zero"
  (Sfixn _,  Sfloat 0) -> Left "Divide by zero"
  (Sfloat _, Sfloat 0) -> Left "Divide by zero"
  (Sfixn a, Sfixn b) -> if a `rem` b == 0 then Right . Sfixn $ a `quot` b
                        else Right . Sfloat $ fromIntegral a / fromIntegral b
  (Sfixn a, Sfloat b) -> Right . Sfloat $ fromIntegral a / b
  (Sfloat a, Sfixn b) -> Right . Sfloat $ a / fromIntegral b
  (Sfloat a, Sfloat b) -> Right . Sfloat $ a / b
  _ -> Left $ "Bad types: " ++ show (Slist [s1,s2])


p_div :: Sval
p_div = Sprim $ \vs _ -> return $ do
  tc_fail (and . map tc_num) vs
  tc_fail (not . null) vs
  foldM s_div (head vs) (tail vs)

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

p_mk_self_ref :: Sval
p_mk_self_ref = Sprim $ \vs i -> return $ case vs of
  [] -> Right $ Sref i
  _ -> len_fail 0 vs

p_mk_ref :: Sval
p_mk_ref = Sprim $ \vs _ -> case vs of
  [] -> randomIO >>= return . Right . Sref
  _ -> return $ len_fail 0 vs

f_as :: Sval
f_as = Sform $ \vs i -> case vs of
  [k, x] -> do
    k' <- _eval k i
    case k' of Right (Sref i') -> _eval x i'
               Right v -> return . Left $ "Bad type (expected ref): " ++ show v
               err -> return err
  _ -> return $ len_fail 2 vs

f_if :: Sval
f_if = Sform $ \vs f -> case vs of
  [cond, y, n] -> do
    v <- _eval cond f
    case v of Left err -> return $ Left err
              Right v' -> case v' of (Sbool False) -> _eval n f
                                     _             -> _eval y f
  _ -> return . Left $ "if: bad conditional syntax: " ++ show (Slist $ sym_if:vs)

f_begin :: Sval
f_begin = Sform $ \sv f -> foldl (>>) (return . return $ Slist []) $ map (\o -> _eval o f) sv

p_arity :: Sval
p_arity = Sprim $ \sv _ -> case sv of
  [Sfunc as _ _] -> return . Right . Sfixn . length $ let splat = bs_splat in takeWhile (\s -> s /= splat) as
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

                      -- only keep the frame if there's a chance we'll need it later
                      case res of Left _ -> dropFrame n >> return res
                                  Right r  -> when (not $ tc_macro r || tc_func r || tc_list r) (dropFrame n) >> return res


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
                             -- this is what less insane lisps call `macroexpansion time.' haha.
                             case expn of Left err -> return $ Left err
                                          Right xv -> _eval xv f

    _apply _ = error "_eval: _apply: unexpected pattern"

    specialForms = M.fromList $
      [ (pack "define",     f_define)
      , (pack "begin",      f_begin )
      , (pack "quote",      f_quote )
      , (pack "if",         f_if    )
      , (pack "lambda",     f_lambda)
      , (pack "set!",       f_set   )
      , (pack "splice",     f_splice)
      , (pack "macro",      f_macro )
      , (pack "unset!",     f_unset )
      , (pack "as",         f_as    )
      , (pack "quasiquote", f_quasiq)
      ]

evalList :: [Sval] -> Int -> IO (Either String [Sval])
evalList vs f = liftM sequence $ mapM (\o -> _eval o f) vs

{- bytestring constants -}

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

-- type and argument checking

tc_str :: Sval -> Bool
tc_str s = case s of { Sstring _ -> True; _ -> False }

tc_num :: Sval -> Bool
tc_num (Sfixn _)  = True
tc_num (Sfloat _) = True
tc_num _          = False


tc_bool :: Sval -> Bool
tc_bool s = case s of { Sbool _ -> True; _ -> False }

tc_func :: Sval -> Bool
tc_func s = case s of { Sfunc _ _ _ -> True; _ -> False }

tc_macro :: Sval -> Bool
tc_macro s = case s of { Smacro _ _ _ -> True; _ -> False }

tc_world :: Sval -> Bool
tc_world s = case s of { Sworld _ -> True; _ -> False }

tc_ref :: Sval -> Bool
tc_ref s = case s of { Sref _ -> True; _ -> False }

tc_handle :: Sval -> Bool
tc_handle s = case s of { Shandle _ -> True; _ -> False }

tc_sym :: Sval -> Bool
tc_sym s = case s of { Ssym _ -> True; _ -> False }

tc_list :: Sval -> Bool
tc_list s = case s of { Slist _ -> True; _ -> False }

tc_prim :: Sval -> Bool
tc_prim s = case s of { Sprim _ -> True; _ -> False }

tc_form :: Sval -> Bool
tc_form s = case s of { Sform _ -> True; _ -> False }

-- | Cause a computation in Either to fail when a (type) predicate returns
-- false.
tc_fail :: Show a => (a -> Bool) -> a -> Either String ()
tc_fail p v = if p v then return () else Left $ "Bad type: " ++ show v

-- | Generate a generic `wrong number of arguments' error.
len_fail :: Int -> [a] -> Either String b
len_fail n l = Left $ "Wrong number of arguments: " ++ show (length l) ++ " for " ++ show n

{- GARBAGE COLLECTION -}

-- | Create a graph representing the state of the lisp environment.
envGraph :: IO (Graph, (Vertex -> ((), Int, [Int])), (Int -> Maybe Vertex))
envGraph = do
  frames <- H.toList env
  return . graphFromEdges $ map es frames
  where es (n,(bs,p)) = ((), n, parent p ++ refs (M.elems bs))
        parent = maybe [] return
        refs = map frameNo . getRefs . Slist
        getRefs (Slist l) = filter (\o -> tc_func o || tc_macro o) l ++ concatMap getRefs l
        getRefs _ = []

-- | General case naive garbage collection. Drops all frames not reachable
-- from toplevel.
gc :: IO ()
gc = do
  (g, v2i, i2v) <- envGraph
  let tv = case i2v toplevel of { Just v -> v; Nothing -> error "gc: rilly messed up env!" }
      getKey = (\(_,k,_) -> k) . v2i
      rs = reachable g tv
      vs = map getKey $ vertices g
      ks = map getKey rs
  mapM_ (H.delete env) (vs \\ ks)


-- | GC algorithm modified to preserve frames belonging to or reachable from
-- object nodes.
gcW :: World -> IO ()
gcW w = do
  (g, v2i, i2v) <- envGraph
  let getKey = (\(_,k,_) -> k) . v2i
      getVec i = case i2v i of { Just v -> v; Nothing -> error "gcW: messed up env!" }
      rs = concatMap (reachable g) $ map getVec (toplevel:(objIds w))
      vs = map getKey $ vertices g
      ks = map getKey rs
  mapM_ (H.delete env) (vs \\ ks)
  where
    depth o = o:(contents o) ++ (concatMap depth $ contents o)
    breadth = concatMap depth
    objIds (f,c) = map objId $ breadth (f:c)

