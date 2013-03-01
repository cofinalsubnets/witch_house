module Gretel.World.Operations
( NodeOp2
, Predicate2
, find2
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
, addNode -- TODO: don't export this!
) where

import Data.Maybe
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as S
import qualified Data.Map as M
import Gretel.World.Node
import Gretel.World.Types

type NodeOp2 = Name -> Name -> WorldTransformer Bool
type Predicate2 = Name -> Name -> World -> Bool

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

-- | Attempt to move a node to a node adjacent to its container.
goes :: NodeOp2
n `goes` dir = \w ->
  let node = M.lookup n w
      loc  = do n <- node
                l <- location n
                loc <- M.lookup l w
                dir `from` loc
  in case loc of Nothing -> (False,w)
                 Just l  -> n `goesTo` l $ w

-- | Insert a node into another node.
takes :: NodeOp2
n1 `takes` n2 = \w ->
  let check = do (n1',n2') <- find2 n1 n2 w
                 return $ location n1' == location n2'
  in if fromMaybe False check 
     then n2 `goesTo` n1 $ w
     else (False,w)

-- | Adjoin a node to another node in the given direction.
-- The connection is unidirectional. Establishing bidirectional
-- adjacency requires another call to adjoins.
adjoins :: Name -> Name -> Direction -> WorldTransformer Bool
n1 `adjoins` n2 = \d w ->
  case find2 n1 n2 w of
    Nothing -> (False,w)
    Just (n1',n2') ->
      let n1'' = n1' { edges = M.insert d n2 (edges n1') }
      in (True, M.insert n1 n1'' w)

-- | Set the description field of a node.
describes :: NodeOp2
desc `describes` node = \w ->
  case M.lookup node w of
    Nothing -> (False,w)
    Just n -> let n' = n { description = desc }
      in (True, M.insert node n' w)

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

-- | Relocate a node in a world.
-- Multiple nodes (at least two, but usually three) must be updated to
-- accomplish this without introducing inconsistencies. The function
-- will not return an updated state unless all the updates are successfull.
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

-- | Insert a node into the world, adjusting the contents of its container
-- (if it has one) appropriately.
addNode :: Node -> World -> World
addNode n w = let w' = (name >>= M.insert $ n) w
  in case location n of Nothing -> w'
                        Just l  -> case M.lookup l w' of
                                     Nothing -> w'
                                     Just ln -> M.insert l (ln { contents = S.insert (name n) (contents ln) }) w'

