{-# LANGUAGE GADTs, FlexibleInstances #-}
module WitchHouse.Types
( Options(..)
, Verbosity(..)
, Obj(..)
, World
, WT
, Scope(..)
, Sval(..)
, Env
, Frame
) where

import Data.Map (Map)
import System.IO (Handle)
import Data.Function (on)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.HashTable.IO (BasicHashTable)
import Data.Set (Set)

-- command line options
data Options = Options { portNo       :: Int
                       , dbPath       :: FilePath
                       , persistent   :: Bool
                       , autosave     :: Int
                       , initialState :: ()
                       , verbosity    :: Verbosity
                       , logHandle    :: Handle
                       }

data Verbosity = V0 | V1 | V2 deriving (Show,Eq,Ord)

-- | A thing in the world.
-- A world is basically a tree of Objs, which we traverse using a zipper.
data Obj = Obj { objId    :: Int
               , exits    :: Map String Int
               , contents :: [Obj]
               , owners   :: Set Int
               , start    :: Bool
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

type Frame = (Map ByteString Sval, Maybe Int)
type Env = BasicHashTable Int Frame

type Htrans = [Sval] -> Int -> IO (Either String Sval)
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
          | Shandle Handle
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
  show (Sworld (f,_)) = "#<obj:" ++ show (objId $ f) ++ ">"
  show (Shandle h) = show h
  show (Sform _) = "#<form>"

instance Eq Sval where
  (Snum a)    == (Snum b)    = a == b
  (Sstring a) == (Sstring b) = a == b
  (Sbool a)   == (Sbool b)   = a == b
  (Ssym a)    == (Ssym b)    = a == b
  (Slist a)   == (Slist b)   = a == b
  (Sworld a)  == (Sworld b)  = (objId.fst $ a) == (objId.fst $ b)
  (Shandle a) == (Shandle b) = a == b
  (Sfunc a b c) == (Sfunc d e f) = (a,b,c) == (d,e,f)
  (Smacro a b c) == (Smacro d e f) = (a,b,c) == (d,e,f)
  _ == _ = False

