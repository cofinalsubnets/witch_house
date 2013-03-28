module WitchHouse.Wisp.Predicates
( guard
, guard'
, noop
, numP
, strP
, symP
, boolP
, worldP
, refP
, handleP
, listP
, primP
, macroP
, funcP
, ArgSpec(..)
, (|||)
, (&&&)
) where

import WitchHouse.Types

type Predicate = Sval -> Bool

strP (Sstring _) = True
strP _ = False

numP (Sfixn _)  = True
numP (Sfloat _) = True
numP _ = False

boolP (Sbool _) = True
boolP _ = False

funcP Sfunc{} = True
funcP _ = False

macroP Smacro{} = True
macroP _ = False

worldP (Sworld _) = True
worldP _ = False

refP (Sref _) = True
refP _ = False

handleP (Shandle _) = True
handleP _ = False

symP (Ssym _) = True
symP _ = False

listP (Slist _) = True
listP _ = False

primP (Sprim _) = True
primP _ = False

guard' as tg = Sprim . guard as tg

guard :: ArgSpec -> [Predicate] -> ([Sval] -> Int -> IO (Either String Sval)) -> [Sval] -> Int -> IO (Either String Sval)
guard as tg fn vs
  | Left err <- lc as vs = const . return $ Left err
  | and $ zipWith ($) tg vs = fn vs
  | otherwise = const $ return typeError
  where typeError = Left $ "ERROR: bad type: " ++ show (Slist vs)

noop = const True

lc :: ArgSpec -> [Sval] -> Either String ()
lc spec vs
  | Any <- spec    = return ()
  | Exactly n <- spec
  , length vs == n = return ()
  | AtMost n <- spec
  , length vs <= n = return ()
  | AtLeast n <- spec
  , length vs >= n = return ()
  | otherwise = Left $ "ERROR: wrong number of arguments: " ++ show (length vs) ++ " for " ++ show spec

p1 ||| p2 = \s -> p1 s || p2 s
p1 &&& p2 = \s -> p1 s && p2 s

data ArgSpec = Exactly Int | AtLeast Int | AtMost Int | Any

instance Show ArgSpec where
  show (Exactly n) = "exactly "  ++ show n
  show (AtLeast n) = "at least " ++ show n
  show (AtMost n)  = "at most "  ++ show n

