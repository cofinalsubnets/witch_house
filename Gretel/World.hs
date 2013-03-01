module Gretel.World
( World
, NodeOp2
, Predicate2
, has
, goes
, takes
, makes
, enters
, leaves
, goesTo
, drops
, adjoins
, describes
, mkWorld
, addNode
, testWorld
, module Gretel.Node
) where

import Gretel.Node
import Gretel.Types
import Data.Maybe
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as S
import qualified Data.Map as M

from :: Direction -> Node -> Maybe Name
dir `from` node = M.lookup dir $ edges node

find2 n1 n2 w = do n1' <- M.lookup n1 w 
                   n2' <- M.lookup n2 w
                   return (n1',n2')

has :: Predicate2
n1 `has` n2 = \w ->
  let check = do (n1',n2') <- find2 n1 n2 w
                 loc <- location n2'
                 return $ loc == name n1'
  in fromMaybe False check

goes :: NodeOp2
n `goes` dir = \w ->
  let node = M.lookup n w
      loc  = do n <- node
                l <- location n
                loc <- M.lookup l w
                dir `from` loc
  in case loc of Nothing -> (False,w)
                 Just l  -> n `goesTo` l $ w

takes :: NodeOp2
n1 `takes` n2 = \w ->
  let check = do (n1',n2') <- find2 n1 n2 w
                 return $ location n1' == location n2'
  in if fromMaybe False check 
     then n2 `goesTo` n1 $ w
     else (False,w)

adjoins :: Direction -> NodeOp2
n1 `adjoins` n2 = \d w ->
  case find2 n1 n2 w of
    Nothing -> (False,w)
    Just (n1',n2') ->
      let n1'' = n1' { edges = M.insert d n2 (edges n1') }
      in (True, M.insert n1 n1'' w)
               
describes :: String -> NodeOp2
n1 `describes` n2 = \d w ->
  case find2 n1 n2 w of
    Nothing -> (False,w)
    Just (n1',n2') ->
      let n2'' = n2' { description = d }
      in (True, M.insert n2 n2'' w)

enters :: NodeOp2
n1 `enters` n2 = n2 `takes` n1

leaves :: NodeOp2
n1 `leaves` n2 = \w ->
  let dest = do (n1',n2') <- find2 n1 n2 w
                if location n1' == Just n2
                then location n2'
                else Nothing
  in case dest of Nothing -> (False,w)
                  Just d  -> n1 `goesTo` d $ w

drops :: NodeOp2
n1 `drops` n2 = n2 `leaves` n1

goesTo :: NodeOp2
n1 `goesTo` n2 = \w ->
  let upd = do (n1',n2') <- find2 n1 n2 w
               l1  <- location n1'
               l1' <- M.lookup l1 w
               let n1'' = n1' { location = Just n2 }
                   n2'' = n2' { contents = S.insert n1 (contents n2') }
                   l1'' = l1' { contents = S.delete n1 (contents l1') }
               return $ addNode l1'' $ addNode n2'' $ addNode n1'' w
  in case upd of Nothing -> (False,w)
                 Just w' -> (True,w')

makes :: NodeOp2
n1 `makes` n2 = \w ->
  case M.lookup n2 w of
    Nothing -> let newNode = mkNode { name = n2
                                    , location = M.lookup n1 w >>= location
                                    }
               in (True,addNode newNode w)
    Just _ -> (False,w)

mkWorld :: World
mkWorld = M.fromList []

addNode :: Node -> World -> World
addNode n w = let w' = (name >>= M.insert $ n) w
  in case location n of Nothing -> w'
                        Just l  -> case M.lookup l w' of
                                     Nothing -> w'
                                     Just ln -> M.insert l (ln { contents = S.insert (name n) (contents ln) }) w'

testWorld :: World
testWorld = let root = mkNode { name = "Root of the World"
                              , description = "\"For the leaves to touch the sky, the roots much reach deep into hell.\"\n  --Thomas Mann"
                               , contents = S.fromList ["feivel"]
                              }
                feivel = mkNode { name = "feivel"
                                , description = "crusty kid w/ broken glasses"
                                , location = Just "Root of the World"
                                }
  in addNode feivel  . addNode root $ mkWorld

