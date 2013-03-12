module WitchHouse.Wisp.Types
( Frame
, Env
, Expr(..)
, Sval(..)
) where

import Data.Map (Map)

type Frame = Map String Sval
type Env = [Frame]

newtype Expr a = Expr { run :: Env -> (a, Env) }

data Sval = Snum Int |
            Sstring String |
            Ssym String |
            Slist [Sval] |
            Sbool Bool |
            Sfunc [String] [Sval] Env |
            Sform [String] [Sval] |
            Sprim ([Sval] -> Env -> (Either String Sval, Env))

instance Show Sval where
  show (Snum n)    = show n
  show (Sstring s) = show s
  show (Sbool b)   = if b then "#t" else "#f"
  show (Ssym s)    = s
  show (Slist l)   = "(" ++ (unwords . map show $ l) ++ ")"
  show (Sfunc _ _ _) = "#<fn>"
  show (Sprim _) = "#<prim fn>"

instance Eq Sval where
  (Snum a)    == (Snum b)    = a == b
  (Sstring a) == (Sstring b) = a == b
  (Sbool a)   == (Sbool b)   = a == b
  (Ssym a)    == (Ssym b)    = a == b
  (Slist a)   == (Slist b)   = a == b
  _ == _ = False

instance Monad Expr where
  return v = Expr $ \e -> (v,e)
  a >>= b = Expr $ \e0 -> let (v1,e1) = run a e0 in run (b v1) e1

