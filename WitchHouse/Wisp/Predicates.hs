module WitchHouse.Wisp.Predicates
( taking
, anyValue
, number
, string
, symbol
, bool
, world
, ref
, port
, list
, primitive
, macro
, function
, applicable
, integer
, float
, strings
, numbers
, integers
, floats
, bools
, functions
, macros
, worlds
, refs
, ports
, symbols
, lists
, primitives
, arguments
, ArgSpec(..)
, (|||)
) where

import WitchHouse.Types

type Pred = Val -> Bool

anyValue = const True

string (Str _) = True
string _ = False

integer (Int _) = True
integer _ = False

float (Flt _) = True
float _ = False

number = integer ||| float

bool (Bln _) = True
bool _ = False

function Fn{isMacro = m} = not m
function _ = False

macro Fn{isMacro = m} = m
macro _ = False

applicable = function ||| macro ||| primitive

world (Wd _) = True
world _ = False

ref (Ref _) = True
ref _ = False

port (Prt _) = True
port _ = False

symbol (Sym _) = True
symbol _ = False

list (Lst _) = True
list _ = False

primitive (Primitive _) = True
primitive _ = False

strings = repeat string
numbers = repeat number
integers = repeat integer
floats = repeat float
bools = repeat bool
functions = repeat function
macros = repeat macro
worlds = repeat world
refs = repeat ref
ports = repeat port
symbols = repeat symbol
lists = repeat list
primitives = repeat primitive
arguments = repeat anyValue

taking :: ArgSpec -> WispFn -> WispFn
taking as fn vs
  | Left err <- guard as vs = \_ -> return $ Left err
  | otherwise = fn vs

guard :: ArgSpec -> [Val] -> Either String ()
guard spec vs
  | AnyNumber gs <- spec = validate gs
  | Exactly n gs <- spec
  , length vs == n = validate gs
  | AtMost n gs <- spec
  , length vs <= n = validate gs
  | AtLeast n gs <- spec
  , length vs >= n = validate gs
  | otherwise = Left $ "ERROR: wrong number of arguments: " ++ show (length vs) ++ " for " ++ show spec
  where validate gs = if and $ zipWith ($) gs vs then return ()
                      else Left $ "ERROR: bad type: " ++ show (Lst vs)

p1 ||| p2 = \s -> p1 s || p2 s

data ArgSpec = Exactly Int [Pred] | AtLeast Int [Pred] | AtMost Int [Pred] | AnyNumber [Pred]

instance Show ArgSpec where
  show (Exactly n _) = "exactly "  ++ show n
  show (AtLeast n _) = "at least " ++ show n
  show (AtMost n _)  = "at most "  ++ show n
  show (AnyNumber _) = "any number"

