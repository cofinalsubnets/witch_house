module WitchHouse.World.Core
( take
, go
, drop
, enter
, exit
, find -- probably shouldn't export this
, find'
, link
, unlink
, matchName
, zDn -- stop exporting these!
, zUp
, zDel
, zUp'
, zIns
, focus
, context
, owns
, make
, mkObj
, name
, description
, password
, handle
) where

import Data.List hiding (find, take, drop)
import qualified Data.List as List (find)
import Prelude hiding (take, drop)
import Control.Monad (liftM, (>=>))
import qualified Data.Map as M
import qualified Data.Set as S (member, fromList)
import WitchHouse.Wisp (toplevel, pushFrame, envLookup, bind)
import System.IO
import System.IO.Unsafe
import Data.ByteString.Char8 (pack)

import WitchHouse.Types

{- CONSTRUCTORS & HIGH-LEVEL TRANSFORMS -}

-- | Insert a new obj into the world with the given name.
make :: String -> World -> IO World
make n w = do
  o <- mkObj
  bind (objId o) (pack "*name*") (Sstring n)
  return . zUp' $ zIns o {owners = S.fromList [objId $ focus w]} w

-- | Create a new obj. In IO because we need to grab a new Unique identifier.
mkObj :: IO Obj
mkObj = do
  n <- pushFrame (M.fromList [], Just toplevel)
  return Obj { objId       = n
             , exits       = M.fromList []
             , contents    = []
             , owners      = S.fromList []
             , start       = False
             }

owns :: Obj -> Obj -> Bool
o1 `owns` o2 = objId o1 `S.member` owners o2

move :: (Obj -> Bool) -> Scope -> WT
move _ _ (_,[]) = Left "You can't move this object."
move p d w = let (o,w') = zPluck w in find p d w' >>= return . zIns o

take :: (Obj -> Bool) -> WT
take p w = find p Location w >>= enter (==focus w)

drop :: (Obj -> Bool) -> WT
drop p = find p Self >=> exit

enter :: (Obj -> Bool) -> WT
enter p = move p Self -- it's counterintuitive that the scope here should be `Self'

go :: String -> WT
go dir w = do
  w' <- zUp w
  case M.lookup dir (exits . focus $ w') of
    Nothing -> Left "You can't go that way."
    Just u  -> move ((== u) . objId) Global w

exit :: WT
exit (_,[]) = Left "You can't exit this location."
exit w = let (o,w') = zPluck w in zUp w' >>= return . zIns o

link :: String -> (Obj -> Bool) -> WT
link dir p w@(orig,c) = do
  dest <- liftM focus $ find p Global w
  return $ (orig{exits = M.insert dir (objId dest) (exits orig)},c)

unlink :: String -> WT
unlink dir (f,c) = return (f{exits = M.delete dir (exits f)},c)

{- SELECTORS & LOW-LEVEL TRANSFORMS -}

focus :: World -> Obj
focus = fst

context :: World -> [Obj]
context = snd

zIns :: Obj -> World -> World
zIns o (r,c) = (o, r:c)

zPluck :: World -> (Obj,World)
zPluck w = (focus w, zDel w)

zDel :: World -> World
zDel (_,[]) = error $ "zDel: attempted to delete root"
zDel (_,c:cs) = (c,cs)

zUp :: WT
zUp (_,[]) = Left "You can't exit this location."
zUp (r,c:cs) = Right (c{contents = r:(contents c)}, cs)

zUp' :: World -> World
zUp' w = case zUp w of Right w' -> w'
                       Left _   -> error "zUp': hit root"

zDn :: World -> [World]
zDn (r,cs) = [(nr,r{contents = delete nr (contents r)}:cs) | nr <- contents r]

-- | Propagate a `wave' in a zipper.
-- A wave originating at some focus propagates up the zipper to the focus'
-- parent, and down to each of its children. We represent a wave as a 2-tuple
-- of wavefronts, upward and downward respectively. The `null' values for each
-- wavefront (Nothing for the upward front, [] for the downward front)
-- represent the case where that front has passed beyond the limits of the
-- data structure.
ripple :: (Maybe World,[World]) -> (Maybe World,[World])
ripple (Just u@(_,_:_),ds) = let u' = zUp' u in (Just u', (delete u $ zDn u') ++ concatMap zDn ds) -- propagate up and down
ripple (_,ds) = (Nothing, concatMap zDn ds) -- we're finished propagating upward but not downward

-- | List zipper configurations in ascending order of distance from the focus.
-- Include configurations up to first argument steps away (unlimited if the
-- first argument is Nothing).
steps :: Maybe Int -> World -> [World]
steps n w = step n (Just w, zDn w)
  where step _ (Nothing,[]) = []
        step (Just 0) l = case fst l of Nothing -> []
                                        Just h -> [h]
        step i l = let c = case fst l of Nothing -> snd l
                                         Just h  -> h:(snd l)
                   in c ++ step (fmap (subtract 1) i) (ripple l)

-- | Find the closest node to the current focus that satisfies the given
-- predicate. The second argument determines the scope of the search:
-- the current focus' own contents (Self); the contents of the location of the
-- current focus (Location); all nodes within some distance of the current
-- focus (Distance n); or all nodes, everywhere (Global).
find :: (Obj -> Bool) -> Scope -> WT
find p s w = case List.find (p.focus) $ ops of Nothing -> Left "I don't know what you're talking about."
                                               Just w' -> Right w'
  where ops = case s of Self -> zDn w
                        Location -> case zUp w of Left _ -> []
                                                  Right l -> delete w $ zDn l
                        Distance i -> steps (Just i) w
                        Global -> steps Nothing w

-- | Like find, but returns an un-Either'd result (and errors if no matching
-- node is found).
find' :: (Obj -> Bool) -> Scope -> World -> World
find' p s w = case find p s w of Right w' -> w'
                                 Left _ -> error "find': search failed"

name :: Obj -> String
name o = unsafePerformIO $ do
  n <- envLookup (pack "*name*") (Just $ objId o)
  return $ case n of Right (Sstring s) -> s
                     _ -> ""

description :: Obj -> String
description o = unsafePerformIO $ do
  n <- envLookup (pack "*desc*") (Just $ objId o)
  return $ case n of Right (Sstring s) -> s
                     _ -> ""

password :: Obj -> Maybe String
password o = unsafePerformIO $ do
  n <- envLookup (pack "*password*") (Just $ objId o)
  return $ case n of Right (Sstring s) -> Just s
                     _ -> Nothing

handle :: Obj -> Maybe Handle
handle o = unsafePerformIO $ do
  n <- envLookup (pack "*handle*") (Just $ objId o)
  return $ case n of Right (Shandle h) -> Just h
                     _ -> Nothing

{- OBJECT PREDICATES -}

matchName :: String -> Obj -> Bool
matchName n = isPrefixOf n . name

