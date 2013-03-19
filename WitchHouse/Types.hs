{-# LANGUAGE GADTs, FlexibleInstances #-}
module WitchHouse.Types
( Options(..)
, Verbosity(..)
, Obj(..)
, World
, WT
, Scope(..)
, Sval(..)
, Expr(..)
, Env
, Frame
) where

import Data.Unique (Unique)
import Data.Map (Map)
import System.IO (Handle)
import Data.Function (on)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)

-- command line options
data Options = Options { portNo       :: Int
                       , dbPath       :: FilePath
                       , persistent   :: Bool
                       , autosave     :: Int
                       , initialState :: World
                       , verbosity    :: Verbosity
                       , logHandle    :: Handle
                       }

data Verbosity = V0 | V1 | V2 deriving (Show,Eq,Ord)

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

data Scope = Self
           | Location
           | Distance Int
           | Global deriving (Show,Eq,Ord)

-- TYPES FOR WISP

type Frame = (Map ByteString Sval, Int)
type Env = Map Int Frame

data Expr m a = Monad m => Expr { run :: Env -> IO (m a, Env) }

type Htrans = [Sval] -> Int -> Env -> IO (Either String Sval, Env)
-- type for wisp values
data Sval = Snum    Int
          | Sstring String
          | Ssym    ByteString
          | Slist   [Sval]
          | Sbool   Bool
          | Sfunc   { params :: [ByteString], body :: [Sval], frameNo :: Int }
          | Smacro  { params :: [ByteString], body :: [Sval], frameNo :: Int }
          | Sform   { transform :: Htrans }
          | Sprim   { transform :: Htrans }
          | Sworld  World

instance Show Sval where
  show (Snum n)    = show n
  show (Sstring s) = show s
  show (Sbool b)   = if b then "#t" else "#f"
  show (Ssym s)    = unpack s
  show (Slist l)   = "(" ++ (unwords . map show $ l) ++ ")"
  show (Sfunc as b f) = concat ["(lambda ", show . Slist $ map Ssym as, " ", unwords $ map show b, ") ;; ", show f]
  show (Smacro as b f) = concat ["(macro ", show . Slist $ map Ssym as, " ", unwords $ map show b, ") ;; ", show f]
  show (Sprim _) = "#<prim fn>"
  show (Sworld _) = "#<world>"
  show (Sform _) = "#<form>"

instance Eq Sval where
  (Snum a)    == (Snum b)    = a == b
  (Sstring a) == (Sstring b) = a == b
  (Sbool a)   == (Sbool b)   = a == b
  (Ssym a)    == (Ssym b)    = a == b
  (Slist a)   == (Slist b)   = a == b
  (Sworld a)  == (Sworld b)  = (objId.fst $ a) == (objId.fst $ b)
  (Sfunc a b c) == (Sfunc d e f) = (a,b,c) == (d,e,f)
  (Smacro a b c) == (Smacro d e f) = (a,b,c) == (d,e,f)
  _ == _ = False

instance Monad (Expr (Either String)) where
  return v = Expr $ \e -> return (return v,e)
  a >>= b = Expr $ \e0 -> do (v1,e1) <- run a e0 
                             case v1 of Left err -> return (Left err,e1)
                                        Right v -> run (b v) e1

