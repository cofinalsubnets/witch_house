{-# LANGUAGE TupleSections #-}
module WitchHouse.World
( Obj(..)
, World
, Scope(..)
, Action(..)
, setName
, setDesc
, make
, take
, go
, drop
, enter
, exit
, move
, find -- probably shouldn't export this
, find'
, remove
, link
, unlink
, object -- this is redundant
, matchName
, mkWorld
, mkObj
, zDn -- stop exporting these!
, zUp
, zUp'
, zIns
, focus
, context
) where

import Data.List hiding (find, take, drop)
import qualified Data.List as List (find)
import Prelude hiding (take, drop)
import Data.Function (on)
import Data.Unique
import Control.Monad (liftM)
import Data.Map (Map)
import System.IO
import qualified Data.Map as M
import Data.Monoid
import WitchHouse.Wisp.Types
import WitchHouse.Wisp


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

newtype Action = Action { execute :: World -> IO World }

instance Monoid Action where
  mempty = Action $ \w -> return w
  mappend a1 a2 = Action $ \w -> execute a1 w >>= execute a2

data Scope = Self | Location | Distance Int | Global deriving (Show,Eq,Ord)

{- CONSTRUCTORS & HIGH-LEVEL TRANSFORMS -}

setName :: String -> WT
setName n (f,c) = return (f{name=n},c)

setDesc :: String -> WT
setDesc d (f,c) = return (f{description=d},c)

-- | Insert a new obj into the world with the given name.
make :: String -> World -> IO World
make n w = mkObj >>= \o -> return . zUp' $ zIns o {name = n} w

move :: (Obj -> Bool) -> Scope -> WT
move _ _ (_,[]) = Left "You can't move this object."
move p d w = let (o,w') = zPluck w in find p d w' >>= return . zIns o

take :: (Obj -> Bool) -> WT
take p w = find p Location w >>= enter (==focus w)

drop :: (Obj -> Bool) -> WT
drop p w = find p Self w >>= exit

remove :: World -> World
remove = zDel

enter :: (Obj -> Bool) -> WT
enter p w = move p Self w -- it's counterintuitive that the scope here should be `Self'

go :: String -> WT
go dir w = do
  w' <- zUp w
  case M.lookup dir (exits . focus $ w') of
    Nothing -> Left "You can't go that way."
    Just u  -> move (object u) Global w

exit :: WT
exit (_,[]) = Left "You can't exit this location."
exit w = let (o,w') = zPluck w in zUp w' >>= return . zIns o

link :: String -> (Obj -> Bool) -> WT
link dir p w@(orig,c) = do
  dest <- liftM focus $ find p Global w
  return $ (orig{exits = M.insert dir (objId dest) (exits orig)},c)

unlink :: String -> WT
unlink dir (f,c) = return (f{exits = M.delete dir (exits f)},c)

-- | Make a new world zipper with the supplied obj at the root.
mkWorld :: Obj -> World
mkWorld = (,[])

-- | Create a new obj. In IO because we need to grab a new Unique identifier.
mkObj :: IO Obj
mkObj = do
  i <- newUnique
  return Obj { name        = ""
             , description = ""
             , objId       = i
             , exits       = M.fromList []
             , contents    = []
             , handle      = Nothing
             , password    = Nothing
             , start       = False
             , bindings    = toplevelBindings
             }

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
find p s w = case List.find (p.focus) $ ops of Nothing -> Left "Huh?"
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

evalWisp :: String -> WT
evalWisp s (o@(Obj {bindings = b}),cs) = case runWisp s b of (Right _, env) -> Right (o{bindings = env},cs)
                                                             (Left err, _) -> Left err

{- OBJECT PREDICATES -}

-- | An entirely redundant predicate, since equality on objs is defined
-- in terms of their id.
object :: Unique -> Obj -> Bool
object u = (u==) . objId

matchName :: String -> Obj -> Bool
matchName n = isPrefixOf n . name

