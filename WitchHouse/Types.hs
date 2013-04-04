{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module WitchHouse.Types
( Options(..)
, Verbosity(..)
, Obj(..)
, World
, WT
, Scope(..)
, Val(..)
, Frame
, Module
, mempty
, FrameNo
, Env
, WispFn
) where

import Data.Map (Map)
import System.IO (Handle)
import Data.Function (on)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Monoid
import Data.HashTable.IO (BasicHashTable)

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
data Obj = Obj { objId    :: Int
               , exits    :: Map String Int
               , contents :: [Obj]
               , start    :: Bool
               }

instance Eq Obj where
  (==) = (==) `on` objId

type World = (Obj, [Obj])

type WT = World -> Either String World

data Scope = Self
           | Location
           | Distance Int
           | Global deriving (Show,Eq,Ord)

-- TYPES FOR WISP

type Frame = (Map ByteString Val, Maybe FrameNo)
type Env = BasicHashTable FrameNo Frame
type Module = ([(ByteString, Val)], String)
type FrameNo = Int
type WispFn = Params -> FrameNo -> Result
type Result = IO (Either String Val)
type Params = [Val]

-- type for wisp values
data Val = Int  Integer
         | Flt  Double
         | Str  String
         | Sym  ByteString
         | Lst [Val]
         | Bln Bool
         | Fn   { params  :: [Val]
                , body    :: [Val]
                , frameNo :: Int
                , isMacro :: Bool
                }
         | Primitive { transform :: [Val] -> Int -> IO (Either String Val) }
         | Prt Handle
         | Ref  Int
         | Wd World
         -- special forms
         | SFbegin | SFquote | SFif | SFlambda | SFmacro | SFmerge
         | SFset | SFunset | SFqq | SFsplice | SFas | SFdef

instance Show Val where
  show (Int n)       = show n
  show (Flt n)      = show n
  show (Str s)     = show s
  show (Prt h)     = show h
  show (Sym s)        = unpack s
  show (Bln b)       = if b then "#t" else "#f"
  show (Lst l)       = "(" ++ (unwords . map show $ l) ++ ")"
  show Fn{params = p, isMacro = m} = "#<" ++ (if m then "m" else "") ++ "fn/" ++ show (length p) ++ ">"
  show (Primitive _)   = "#<fn/prim>"
  show (Wd _)          = "#<obj>"
  show (Ref _)         = "#<ref>"
  show SFbegin         = "begin"
  show SFqq            = "quasiquote"
  show SFquote         = "quote"
  show SFsplice        = "splice"
  show SFif            = "if"
  show SFset           = "set!"
  show SFunset         = "unset!"
  show SFdef           = "define"
  show SFlambda        = "lambda"
  show SFmacro         = "macro"
  show SFas            = "as"
  show SFmerge         = "msplice"

instance Eq Val where
  Int a == Int b = a == b
  Int a == Flt b = fromIntegral a == b
  Flt a == Int b = a == fromIntegral b
  Flt a == Flt b = a == b
  Str a == Str b = a == b
  Bln a == Bln b = a == b
  Sym a == Sym b = a == b
  Lst a == Lst b = a == b
  Prt a == Prt b = a == b
  Ref a == Ref b = a == b

  Wd (a,_)  == Wd (b,_)  = objId a == objId b

  Fn a b c d  == Fn e f g h  = (a,b,c,d) == (e,f,g,h)

  _ == _ = False

