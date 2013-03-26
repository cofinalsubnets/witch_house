{-# LANGUAGE RankNTypes, PatternGuards #-}
module WitchHouse.Wisp.Core
( eval
, apply
, lookup
, toplevel
, env
, bind
, unbind
, getFrame
, pushFrame
, dropFrame
) where

import Prelude hiding (lookup)
import WitchHouse.Types
import WitchHouse.Wisp.Predicates
import Control.Monad
import qualified Data.Map as M
import qualified Data.HashTable.IO as H
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString (ByteString)
import System.IO.Unsafe
import System.Random

-- what's a functional programming language without global mutable state?
env :: H.BasicHashTable Int Frame
env = unsafePerformIO $ do
  e <- H.new
  H.insert e toplevel (bindings, Nothing)
  return e

toplevel = 0
getFrame = H.lookup env >=> return . maybe (error "getFrame: missing frame") id
dropFrame = H.delete env

pushFrame f = do
  u <- randomIO
  H.insert env u f
  return u

bind f k v = getFrame f >>= \(bs,p) -> H.insert env f (M.insert k v bs, p)
unbind f k = getFrame f >>= \(bs,p) -> H.insert env f (M.delete k bs, p)

lookup :: ByteString -> Maybe Int -> IO (Either String Sval)
lookup s Nothing = return . Left $ "Unable to resolve symbol: " ++ unpack s
lookup s (Just i) = getFrame i >>= \(binds,nxt) -> maybe (lookup s nxt) (return . return) $ M.lookup s binds

bindings :: M.Map ByteString Sval
bindings = M.fromList $
  [ (pack "+",          math (+)   )
  , (pack "-",          math (-)   )
  , (pack "*",          math (*)   )
  , (pack "/",          Sprim $ tc (repeat tc_num) $ some p_div )
  , (pack "=",          Sprim p_eq     )
  , (pack "pi",         Sfloat pi      )
  , (pack "eval",       Sprim $ lc 1 $ eval . head)
  , (pack "cat",        Sprim $ tc (repeat tc_str) p_cat)
  , (pack "apply",      Sprim $ lc 2 p_apply)
  , (pack "string",     Sprim $ lc 1 p_str )
  , (pack "symbol",     Sprim $ tc [tc_str] p_sym )
  , (pack "cons",       Sprim $ lc 2 $ tc [noop, tc_list] $ p_cons)
  , (pack "int",        Sprim $ lc 1 $ tc [tc_num]  $ p_int)
  , (pack "null?",      Sprim $ lc 1 $ tc [tc_list] $ p_null)
  , (pack "error",      Sprim $ lc 1 $ tc [tc_str]  $ p_err)
  , (pack "arity",      Sprim $ lc 1 $ tc [tc_func] $ p_arity)
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
  , (pack "make-ref",   Sprim $ lc 0 p_mk_ref       )
  , (pack "make-self-ref", Sprim $ lc 0 p_mk_self_ref)
  ]

-- PRIMITIVE FN COMBINATORS
-- | variadic math operations. division is handled as a special case.
math :: (forall a. Num a => a -> a -> a) -> Sval
math op = Sprim $ tc (repeat tc_num) $ some _math
  where _math (h:t) _ = return $ foldM (s_num_op op) h t

-- | predicate wrapper for variadic typechecking
check p = Sprim $ \vs -> evalList vs >=> return . fmap (Sbool . all p)

-- PRIMITIVE FUNCTIONS

-- | string coercion
p_str [s@(Sstring _)] _ = return (Right s)
p_str [v] _ = return . Right . Sstring $ show v

-- | equality
p_eq vs _ = return . return . Sbool . and . zipWith (==) vs $ drop 1 vs

-- | cons
p_cons [s, Slist l] _ = return . Right $ Slist (s:l)

-- | apply
p_apply [a, Slist l] = apply a l

-- | integer coercion
p_int [Sfixn n]  _ = return . Right $ Sfixn n
p_int [Sfloat f] _ = return $ Right (Sfixn $ floor f)

-- | raise an error
p_err [Sstring e] _ = return $ Left ("ERROR: " ++ e)

-- | string concatenation
p_cat = const . return . return . Sstring . concatMap (\(Sstring s) -> s)

-- | predicate for null list
p_null [Slist l] _ = return . return . Sbool $ null l

-- | get fn arity
p_arity = const . return . Right . Sfixn . length . takeWhile (/= bs_splat) . params . head

-- | string -> symbol coercion
p_sym [Sstring s] _ = return . Right . Ssym $ pack s

-- | get ref for current frame
p_mk_self_ref _ = return . Right . Sref

-- | get new ref
p_mk_ref _ _ = randomIO >>= return . Right . Sref

-- | division
p_div (h:t) _ = return $ foldM s_div h t

-- Primitive fns and special forms

-- | polymorphic binary math op application. handles coercion between numeric
-- types (basically, float is contagious)
s_num_op :: (forall a. Num a => a -> a -> a) -> Sval -> Sval -> Either String Sval
s_num_op (?) s1 s2 = case (s1,s2) of
  (Sfixn a, Sfixn b)   -> Right . Sfixn  $ a ? b
  (Sfixn a, Sfloat b)  -> Right . Sfloat $ fromIntegral a ? b
  (Sfloat a, Sfixn b)  -> Right . Sfloat $ a ? fromIntegral b
  (Sfloat a, Sfloat b) -> Right . Sfloat $ a ? b
  _ -> Left $ "ERROR: bad type (expected numeric): " ++ show (Slist [s1,s2])

-- stop handling this as a gross special case maybe?
s_div :: Sval -> Sval -> Either String Sval
s_div s1 s2 = case (s1,s2) of
  (_, Sfixn 0) -> db0
  (_, Sfloat 0) -> db0
  (Sfixn a, Sfixn b) -> if a `rem` b == 0 then Right . Sfixn $ a `quot` b
                        else Right . Sfloat $ fromIntegral a / fromIntegral b
  (Sfixn a, Sfloat b) -> Right . Sfloat $ fromIntegral a / b
  (Sfloat a, Sfixn b) -> Right . Sfloat $ a / fromIntegral b
  (Sfloat a, Sfloat b) -> Right . Sfloat $ a / b
  _ -> Left $ "ERROR: /: bad type (expected numeric): " ++ show (Slist [s1,s2])
  where db0 = Left "ERROR: /: divide by zero"

-- | Function application.
apply :: Sval -> [Sval] -> Int -> IO (Either String Sval)
apply sv vs i
 | tc_prim sv = transform sv vs i -- primitive fn application - the easy case!
 | tc_func sv || tc_macro sv = apply posArgs splat vs
 | otherwise = return  . Left $ "ERROR: apply: non-applicable value: " ++ show sv
  where
    (posArgs, splat) = break (== bs_splat) (params sv)
    apply pos var sup

      | not $ null var || length var == 2
      = return . Left $ "ERROR: apply: bad variadic parameter syntax: " ++ show (params sv)

      | length pos > length sup || null var && length pos < length sup
      = return . Left $ "ERROR: wrong number of arguments: " ++ show (length sup) ++ " for " ++ show (length pos)

      | otherwise = do
        let posV = pos `zip` vs
            varV = if null var then [] else [(last var, Slist $ drop (length pos) vs)]
        n   <- pushFrame (M.fromList (varV ++ posV), Just $ frameNo sv)
        res <- f_begin (body sv) n

        -- only keep the frame if there's a chance we'll need it later
        case res of Left _ -> dropFrame n
                    Right r  -> when (not $ any ($r) [tc_macro, tc_func, tc_list]) (dropFrame n)
        return res


eval :: Sval -> Int -> IO (Either String Sval)
eval v f
 | Ssym s <- v = lookup s (Just f)
 | Slist ((Ssym s):t) <- v, Just fn <- M.lookup s specialForms = fn t f
 | Slist (o:vs) <- v = _apply o vs
 | otherwise = return $ return v

  where
    _apply o vs = eval o f >>= \op -> case op of
      Right op' -> if not $ tc_macro op' then evalList vs f >>= \vals ->
                     case vals of Right vals' -> apply op' vals' f
                                  Left err -> return $ Left err
                   else apply op' vs f >>= \expn ->
                     case expn of Right xv -> eval xv f
                                  err -> return err
      err -> return err

evalList :: [Sval] -> Int -> IO (Either String [Sval])
evalList vs f = liftM sequence $ mapM (flip eval f) vs

-- SPECIAL FORMS

specialForms :: M.Map ByteString ([Sval] -> Int -> IO (Either String Sval))
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

f_as = lc 2 $ \[k,x] i ->
  eval k i >>= \k' -> case k' of
    Right (Sref i') -> H.lookup env i' >>= maybe badFrame (const $ eval x i')
    Right v -> badType v
    err -> return err
  where badFrame  = return $ Left "ERROR: as: bad frame identifier"
        badType v = return . Left $ "ERROR: as: bad type (expected ref): " ++ show v

f_if = lc 3 $ \[cond,y,n] f ->
  eval cond f >>= \v -> case v of
    Right (Sbool False) -> eval n f
    Right _             -> eval y f
    err                 -> return err

f_begin sv = fmap (fmap last) . evalList sv

f_quote = lc 1 $ const . return . Right . head

f_quasiq = lc 1 $ \[v] -> case v of Slist l -> spliceL l
                                    sv -> const (return $ Right sv)
  where
    spliceL l f = mapM (splice f) l >>= return . fmap Slist . sequence
    splice f (Slist l@[s,v]) = if s == sym_splice then eval v f else spliceL l f
    splice f (Slist l) = spliceL l f
    splice _ v = return $ return v

f_splice _ _ = return $ Left "ERROR: splice: splice outside of quasiquoted expression"

f_lambda = some $ tc [tc_list] $ \((Slist ps):svs) ->
  let ps' = map (\(Ssym s) -> s) ps in return . return . Sfunc ps' svs

f_macro = some $ tc [tc_list] $ \((Slist ps):svs) ->
  let ps' = map (\(Ssym s) -> s) ps in return . return . Smacro ps' svs

f_define vs f = case vs of
  [Ssym s, xp] -> do
    eval xp f >>= \xv -> case xv of
      Right v -> bind f s v >> return (Right v)
      err -> return err
  (Slist (h:ss)):xps -> f_define [h, Slist ([sym_lambda, Slist ss] ++ xps)] f
  _ -> return $ Left "ERROR: define: bad definition syntax"


f_set = lc 2 $ tc [tc_sym] $ \[Ssym s, xp] f ->
  eval xp f >>= \xv -> case xv of
    Right v -> findBinding s f >>= \f' -> case f' of
      Nothing -> return . Left $ "ERROR: set!: free or immutable variable: " ++ unpack s
      Just n -> bind n s v >> return xv
    err -> return err

f_unset = lc 1 $ tc [tc_sym] $ \[Ssym s] f ->
  findBinding s f >>= \f' -> case f' of
    Nothing -> return . Left $ "ERROR: unset!: free or immutable variable: " ++ unpack s
    Just n -> do (b,_) <- getFrame n
                 let v = b M.! s
                 unbind n s
                 return $ Right v

findBinding :: ByteString -> Int -> IO (Maybe Int)
findBinding nm n
  | n == toplevel = return Nothing
  | otherwise = do (bs,n) <- getFrame n
                   case n of Just n' -> if nm `M.member` bs then return n
                                        else findBinding nm n'
                             Nothing -> return Nothing

-- some bytestring constants
sym_lambda = Ssym $ pack "lambda"
sym_splice = Ssym $ pack "splice"
bs_splat   = pack "."

