{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module WitchHouse.Types
( Options(..)
, Verbosity(..)
, Obj(..)
, World
, WT
, Scope(..)
, Sval(..)
, Frame
) where

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

type Frame = (Map ByteString Sval, Maybe Int)

-- type for wisp values
data Sval = Sfixn   Integer
          | Sfloat  Double
          | Sstring String
          | Ssym    ByteString
          | Slist   [Sval]
          | Sbool   Bool
          | Sfunc   { params :: [ByteString], body :: [Sval], frameNo :: Int }
          | Smacro  { params :: [ByteString], body :: [Sval], frameNo :: Int }
          | Sprim   { transform :: [Sval] -> Int -> IO (Either String Sval) }
          | Shandle Handle
          | Sref    Int
          | Sworld  World
          -- special forms
          | SFbegin | SFquote | SFif | SFlambda | SFmacro | SFmerge
          | SFset | SFunset | SFqq | SFsplice | SFas | SFdef

instance Show Sval where
  show (Sfixn n)       = show n
  show (Sfloat n)      = show n
  show (Sstring s)     = show s
  show (Shandle h)     = show h
  show (Ssym s)        = unpack s
  show (Sbool b)       = if b then "#t" else "#f"
  show (Slist l)       = "(" ++ (unwords . map show $ l) ++ ")"
  show (Sfunc as b f)  = concat ["(lambda ", show . Slist $ map Ssym as, " ", unwords $ map show b, ") ;; ", show f]
  show (Smacro as b f) = concat ["(macro ",  show . Slist $ map Ssym as, " ", unwords $ map show b, ") ;; ", show f]
  show (Sprim _)       = "#<prim fn>"
  show (Sworld (f,_))  = "#<obj:" ++ show (objId $ f) ++ ">"
  show (Sref _)        = "#<ref>"
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

instance Eq Sval where
  (Sfixn a)   == (Sfixn b)   = a == b
  (Sfixn a)   == (Sfloat b)  = fromIntegral a == b
  (Sfloat a)  == (Sfixn b)   = a == fromIntegral b
  (Sfloat a)  == (Sfloat b)  = a == b
  (Sstring a) == (Sstring b) = a == b
  (Sbool a)   == (Sbool b)   = a == b
  (Ssym a)    == (Ssym b)    = a == b
  (Slist a)   == (Slist b)   = a == b
  (Shandle a) == (Shandle b) = a == b
  (Sref a)    == (Sref b)    = a == b

  (Sworld (a,_))  == (Sworld (b,_))  = objId a == objId b

  (Sfunc a b c)  == (Sfunc d e f)  = (a,b,c) == (d,e,f)
  (Smacro a b c) == (Smacro d e f) = (a,b,c) == (d,e,f)

  _ == _ = False

