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
, pure
, trap
) where

import Prelude hiding (lookup)
import WitchHouse.Types
import WitchHouse.Wisp.Predicates
import Control.Monad hiding (guard)
import qualified Data.Map as M
import qualified Data.HashTable.IO as H
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString (ByteString)
import System.IO.Unsafe
import System.Random

pure f ps = return . return . f ps

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

lookup :: ByteString -> Int -> IO (Either String Val)
lookup n = lookup' n . Just
  where
    lookup' s Nothing = return . Left $ "Unable to resolve symbol: " ++ unpack s
    lookup' s (Just i) = getFrame i >>= \(binds,nxt) -> maybe (lookup' s nxt) (return . return) $ M.lookup s binds

bindings :: M.Map ByteString Val
bindings = M.fromList $
  [ (pack "+",          math (+))
  , (pack "-",          math (-))
  , (pack "*",          math (*))
  , (pack "/",          p_div)
  , (pack "=",          p_eq)
  , (pack "eval",       Primitive $ taking (Exactly 1 arguments) (eval . head))
  , (pack "apply",      p_apply)
  , (pack "str",        p_str)
  , (pack "symbol",     p_sym)
  , (pack "int",        p_int)
  , (pack "error",      p_err)
  , (pack "arity",      p_arity)
  , (pack "bool?",      check bool  )
  , (pack "integer?",   check integer)
  , (pack "string?",    check string   )
  , (pack "number?",    check number   )
  , (pack "world?",     check world )
  , (pack "func?",      check function  )
  , (pack "list?",      check list  )
  , (pack "symbol?",    check symbol   )
  , (pack "primitive?", check primitive  )
  , (pack "macro?",     check macro )
  , (pack "port?",    check port)
  , (pack "ref?",       check ref   )
  , (pack "<",          p_lt)
  , (pack "make-ref",      p_mk_ref     )
  , (pack "make-self-ref", p_mk_self_ref)
  ]

-- PRIMITIVE FN COMBINATORS
-- | variadic math operations. division is handled as a special case.
math :: (forall a. Num a => a -> a -> a) -> Val
math op = Primitive
        $ taking (AtLeast 1 numbers)
        $ pure
        $ const . foldl1 (s_num_op op)

-- | predicate wrapper for variadic typechecking
check p = Primitive $ pure $ \vs _ -> Bln $ all p vs

-- PRIMITIVE FUNCTIONS

-- | string coercion
p_str = Primitive
      $ taking (AnyNumber arguments)
      $ pure
      $ \ps _ -> Str $ concatMap stringify ps
  where stringify (Str s) = s
        stringify v = show v

-- | numeric comparison
p_lt = Primitive
     $ taking (Exactly 2 numbers)
     $ pure
     $ \ns _ -> Bln $ case ns of
         [Int a,  Int b] -> a < b
         [Int a,  Flt b] -> fromIntegral a < b
         [Flt a, Int b]  -> a < fromIntegral b
         [Flt a, Flt b]  -> a < b

-- | equality
p_eq = Primitive $ pure $ \vs _ -> Bln . and . zipWith (==) vs $ drop 1 vs

-- | apply
p_apply = Primitive
        $ taking (Exactly 2 [applicable, list])
        $ \[a, Lst l] -> apply a l

-- | integer coercion
p_int = Primitive
      $ taking (Exactly 1 numbers)
      $ pure
      $ const . intg
  where intg [Flt f] = Int $ floor f
        intg [n] = n

-- | raise an error
p_err = Primitive
      $ taking (Exactly 1 strings)
      $ \[Str e] _ -> return . Left $ "ERROR: " ++ e

-- | get fn arity
p_arity = Primitive
        $ taking (Exactly 1 [function ||| macro])
        $ pure
        $ \[Fn{params = p}] _ -> Int . fromIntegral . length $ takeWhile (/= Sym bs_splat) p

-- | string -> symbol coercion
p_sym = Primitive
      $ taking (Exactly 1 strings)
      $ pure
      $ \[Str s] _ -> Sym $ pack s

-- | get ref for current frame
p_mk_self_ref = Primitive $ taking (Exactly 0 arguments) $ pure $ const Ref

-- | get new ref
p_mk_ref = Primitive
         $ taking (Exactly 0 arguments)
         $ \_ _ -> fmap (Right . Ref) randomIO

-- | division
p_div = Primitive
      $ taking (AtLeast 1 numbers)
      $ \(h:t) _ -> return $ foldM s_div h t

-- Primitive fns and special forms

-- | polymorphic binary math op application. handles coercion between numeric
-- types
s_num_op :: (forall a. Num a => a -> a -> a) -> Val -> Val -> Val
s_num_op (?) s1 s2 = case (s1,s2) of
  (Int a, Int b) -> Int $ a ? b
  (Int a, Flt b) -> Flt $ fromIntegral a ? b
  (Flt a, Int b) -> Flt $ a ? fromIntegral b
  (Flt a, Flt b) -> Flt $ a ? b

-- stop handling this as a gross special case maybe?
s_div :: Val -> Val -> Either String Val
s_div s1 s2
 | s2 == Int 0 || s2 == Flt 0 = Left "ERROR: divide by zero"
 | otherwise = case (s1,s2) of
   (Int a, Int b) -> Right . Int $ a `quot` b
   (Int a, Flt b) -> Right . Flt $ fromIntegral a / b
   (Flt a, Int b) -> Right . Flt $ a / fromIntegral b
   (Flt a, Flt b) -> Right . Flt $ a / b

-- | Function application.
apply :: Val -> [Val] -> Int -> IO (Either String Val)
apply sv sup i
  | Primitive f <- sv = f sup i
  | null var && length pos < length sup || null sup && (not $ null pos) = argError
  | otherwise = flip trap (destructure (Lst bound) (Lst sup)) $ \kvs -> do
    n <- pushFrame (M.fromList kvs, Just $ frameNo sv) 
    if null unbound then eval (Lst $ SFbegin:(body sv)) n
    else return . return $ sv{params = unbound, frameNo = n}

  where
    (pos, var) = break (== Sym bs_splat) $ params sv
    (bound, unbound) = if length sup >= length pos then (params sv,[])
                       else splitAt (length sup) (params sv)

    argError = return . Left . unwords $ 
      ["ERROR: wrong number of arguments:"
      , show (length sup)
      , "for"
      , show (length pos)
      ]


eval :: Val -> Int -> IO (Either String Val)
eval v f
 | Sym s <- v = lookup s f
 | Lst (o:vs) <- v = case o of
   SFbegin -> f_begin vs f >>= trap ($())
   SFquote -> f_quote vs f
   SFif -> f_if vs f
   SFlambda -> f_fn False vs f
   SFset -> f_set vs f
   SFsplice -> f_splice vs f
   SFmerge -> f_splice vs f
   SFmacro -> f_fn True vs f
   SFunset -> f_unset vs f
   SFas -> f_as vs f
   SFqq -> f_quasiq vs f
   SFdef -> f_define vs f
   _ -> eval o f >>= trap (_apply vs)
 | otherwise = return $ return v

  where
    _apply vs op = if macro op
                   then apply op vs f >>= trap (flip eval f)
                   else evalList vs f >>= trap (\vals -> apply op vals f)

evalList :: [Val] -> Int -> IO (Either String [Val])
evalList s = fmap sequence . el s
  where 
    el (v:vs) f = eval v f >>= \res -> case res of 
      r@(Right _) -> liftM2 (:) (return r) (el vs f)
      err -> return [err]
    el [] _ = return []

-- SPECIAL FORMS

f_begin [] _ = return . Right . const . return . Right $ Lst []
f_begin sv f = evalList (init sv) f >>= trap (const $ return . Right . const $ eval (last sv) f)

f_as = taking (Exactly 2 arguments) $ \[k,x] i ->
  eval k i >>= \k' -> case k' of
    Right (Ref i') -> H.lookup env i' >>= maybe badFrame (const $ eval x i')
    Right v -> badType v
    err -> return err
  where badFrame  = return $ Left "ERROR: as: not a frame identifier"
        badType v = return . Left $ "ERROR: as: bad type (expected ref): " ++ show v

f_if = taking (Exactly 3 arguments)
     $ \[c,y,n] f -> eval c f >>= trap (\v -> eval (if v == Bln False then n else y) f)

f_quote = taking (Exactly 1 arguments)
        $ pure
        $ const . head

f_quasiq = taking (Exactly 1 arguments) $ \[v] -> case v of
  Lst l -> spliceL l >=> return . fmap Lst . sequence
  sv -> const (return $ Right sv)
  where
    spliceL [] _ = return []
    spliceL ((Lst l):t) f
     | [SFsplice, v] <- l = liftM2 (:) (eval v f) (spliceL t f)
     | [SFmerge,  v] <- l = do
       m <- spliceL [v] f
       case sequence m of
         Right [Lst l'] -> liftM2 (++) (return $ map return l') (spliceL t f)
         Right v' -> liftM2 (:) (return $ Left $ "ERROR: msp: bad merge syntax: " ++ show v') (spliceL t f)
         Left err -> liftM2 (:) (return $ Left err) (spliceL t f)
     | otherwise = liftM2 (:) (fmap (fmap Lst . sequence) $ spliceL l f) (spliceL t f)
    spliceL (v:t) f = liftM2 (:) (return $ return v) (spliceL t f)

f_splice _ _ = return $ Left "ERROR: sp: splice outside of quasiquoted expression"

f_fn m = taking (AtLeast 1 [list])
       $ pure
       $ \((Lst ps):svs) f -> Fn ps svs f m

f_define [s, xp] f = eval xp f >>= trap (return . destructure s) >>= trap bindAll
  where bindAll xpns = mapM_ (\(k,v) -> bind f k v) xpns >> return (return s)
f_define _ _ = return $ Left "ERROR: df: bad definition syntax"


f_set = taking (Exactly 2 [symbol, anyValue]) $ \[Sym s, xp] f ->
  eval xp f >>= trap (\v -> findBinding s f >>= \f' -> case f' of
                              Nothing -> return . Left $ "ERROR: set: free or immutable variable: " ++ unpack s
                              Just n -> bind n s v >> return (return v))

f_unset = taking (Exactly 1 symbols) $ \[Sym s] f ->
  findBinding s f >>= \f' -> case f' of
    Nothing -> return . Left $ "ERROR: undf: free or immutable variable: " ++ unpack s
    Just n -> unbind n s >> return (Right $ Lst [])

findBinding :: ByteString -> Int -> IO (Maybe Int)
findBinding nm f = do
  (bs,n) <- getFrame f
  case n of Just n' -> if nm `M.member` bs then return $ return f
                       else findBinding nm n'
            Nothing -> return Nothing

-- bytestring constants
bs_splat = pack "&"

-- list destructuring
destructure :: Val -> Val -> Either String [(ByteString, Val)]
destructure (Sym s) v = Right [(s,v)]
destructure (Lst l) (Lst v) 
  | (req, (_:o)) <- break (== Sym bs_splat) l = do
    let ps = length req
    pos <- destructure (Lst req) (Lst $ take ps v)
    case o of [s] -> fmap (pos ++) $ destructure s (Lst $ drop ps v)
              s -> Left $ "Pattern error: bad splat: " ++ show (Lst l)
  | length l == length v = fmap concat . sequence $ zipWith destructure l v
  | otherwise = Left . unwords $
    [ "Pattern error: structure mismatch:"
    , show (length l)
    , "pattern(s),"
    , show (length v)
    , "value(s) in"
    , show (Lst l)
    , "<-"
    , show (Lst v)
    ]
destructure l@(Lst _) v = Left . unwords $ 
  [ "Pattern error: structure mismatch: can't match"
  , show l
  , "with"
  , show v
  ]
destructure p _ = Left $ "Pattern error: illegal pattern: " ++ show p

trap f (Right v) = f v
trap _ (Left r) = return $ Left r

