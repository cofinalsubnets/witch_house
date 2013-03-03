module Gretel.Interface.Response
( Response(..)
, Scope(..)
, self
, local
, global
) where

-- TODO: Local should take a list of nodes to notify.
data Scope = Self | Local | Up Int | Down Int | Horiz Int | Global deriving (Show,Eq)

data Response = Response { selfMsg  :: String
                         , otherMsg :: String
                         , scope    :: Scope
                         } deriving (Show,Eq)

self :: String -> Response
self m = Response m "" Self

local :: String -> String -> Response
local s o = Response s o Local

global :: String -> String -> Response
global s o = Response s o Global


