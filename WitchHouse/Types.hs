module WitchHouse.Types
( Obj(..)
, World
, WT
, Scope(..)
, Sval(..)
, Expr(..)
, Env
, Frame
)where

import Data.Unique (Unique)
import Data.Map (Map)
import System.IO (Handle)
import Data.Function (on)

-- | A thing in the world.
-- A world is basically a tree of Objs, which we traverse using a zipper.
data Obj = Obj { name     :: String
               , description :: String
               , objId    :: Unique
               , exits    :: Map String Unique
               , contents :: [Obj]
               , handle   :: Maybe Handle
               , password :: Maybe String
               , start    :: Bool
               , bindings :: Env
               }

instance Eq Obj where
  (==) = (==) `on` objId

-- TODO: data this shit so we can hide the implementation and give it some
-- useful instances. an Eq instance that tests equality of the focus, e.g.
type World = (Obj, [Obj])

type WT = World -> Either String World

data Scope = Self | Location | Distance Int | Global deriving (Show,Eq,Ord)

-- TYPES FOR WISP

type Frame = (Map String Sval, Int)
type Env = Map Int Frame

newtype Expr a = Expr { run :: Env -> (a, Env) }

-- type for wisp values
data Sval = Snum Int |
            Sstring String |
            Ssym String |
            Slist [Sval] |
            Sbool Bool |
            Sfunc [String] [Sval] Int |
            Sform [String] [Sval] |
            Sprim ([Sval] -> Int -> Env -> (Either String Sval, Env)) |
            Sworld World |
            Sactn (World -> IO World)

instance Show Sval where
  show (Snum n)    = show n
  show (Sstring s) = show s
  show (Sbool b)   = if b then "#t" else "#f"
  show (Ssym s)    = s
  show (Slist l)   = "(" ++ (unwords . map show $ l) ++ ")"
  show (Sfunc as b _) = "(lambda " ++ show (Slist $ map Ssym as) ++ " " ++ (unwords $ map show b) ++ ")"
  show (Sprim _) = "#<prim fn>"
  show (Sactn _) = "#<actn>"
  show (Sworld _) = "#<world>"
  show (Sform _ _) = "#<form>"

instance Eq Sval where
  (Snum a)    == (Snum b)    = a == b
  (Sstring a) == (Sstring b) = a == b
  (Sbool a)   == (Sbool b)   = a == b
  (Ssym a)    == (Ssym b)    = a == b
  (Slist a)   == (Slist b)   = a == b
  (Sworld a)  == (Sworld b)  = (objId.fst $ a) == (objId.fst $ b)
  _ == _ = False

instance Monad Expr where
  return v = Expr $ \e -> (v,e)
  a >>= b = Expr $ \e0 -> let (v1,e1) = run a e0 in run (b v1) e1

