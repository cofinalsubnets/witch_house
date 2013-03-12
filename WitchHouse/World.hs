{-# LANGUAGE TupleSections, BangPatterns #-}
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
, run
, prim_apply
, Sval(..)
, envLookup
, evalWisp
) where

import Data.List hiding (find, take, drop)
import qualified Data.List as List (find, take, drop)
import Prelude hiding (take, drop)
import Data.Function (on)
import Data.Unique
import Control.Monad (liftM)
import Data.Map (Map)
import System.IO
import System.IO.Unsafe
import Data.Maybe
import qualified Data.Map as M
import Data.Monoid

-- for the parser:
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)
import Control.Monad


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

-- TYPES FOR WISP

type Frame = Map String Sval
type Env = [Frame]

newtype Expr a = Expr { run :: Env -> (a, Env) }

-- type for wisp values
data Sval = Snum Int |
            Sstring String |
            Ssym String |
            Slist [Sval] |
            Sbool Bool |
            Sfunc [String] [Sval] Env |
            Sform [String] [Sval] |
            Sprim ([Sval] -> Env -> (Either String Sval, Env)) |
            Sworld World |
            Sactn (World -> IO World)

instance Show Sval where
  show (Snum n)    = show n
  show (Sstring s) = show s
  show (Sbool b)   = if b then "#t" else "#f"
  show (Ssym s)    = s
  show (Slist l)   = "(" ++ (unwords . map show $ l) ++ ")"
  show (Sfunc _ _ _) = "#<fn>"
  show (Sprim _) = "#<prim fn>"
  show (Sactn _) = "#<actn>"
  show (Sworld _) = "#<world>"

instance Eq Sval where
  (Snum a)    == (Snum b)    = a == b
  (Sstring a) == (Sstring b) = a == b
  (Sbool a)   == (Sbool b)   = a == b
  (Ssym a)    == (Ssym b)    = a == b
  (Slist a)   == (Slist b)   = a == b
  (Sworld a)  == (Sworld b)  = (objId.focus $ a) == (objId.focus $ b)
  _ == _ = False

instance Monad Expr where
  return v = Expr $ \e -> (v,e)
  a >>= b = Expr $ \e0 -> let (v1,e1) = run a e0 in run (b v1) e1

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

{- WISP
 - a scripting language for witch_house
 -}

runWisp :: String -> Env -> (Either String Sval, Env)
runWisp s e = case parseWisp s of Right sv -> eval' sv e
                                  Left _ -> (Left "Parse error", e)
  where eval' vs env = let (r,env') = run (prim_eval vs) env
                       in case r of Right l@(Slist _) -> eval' l env'
                                    _ -> (r, env')

{- PRIMITIVES -}

prim_lambda :: Sval
prim_lambda = Sprim $ \vs env ->
  case vs of (Slist ps):svs -> let ps' = map (\(Ssym s) -> s) ps
                               in (return $ Sfunc ps' svs env, env)
             _ -> (Left "Malformed lambda exp", env)

prim_define :: Sval
prim_define = Sprim $ \vs env@(f:fs) ->
  case vs of [Ssym s, xp] -> let xv = fst $ run (prim_eval xp) env
                             in case xv of Right v -> (Right v, (M.insert s v f):fs)
                                           Left e -> (Left e, f:fs)
             _ -> (Left "Bad definition syntax", env)

fold_num :: (Int -> Int -> Int) -> Sval
fold_num op = Sprim $ \vs env ->
  let (vs',env') = evalList vs env
      ret = do vals <- vs'
               case fst $ run (prim_apply nump vals) env of
                 Right (Sbool True) -> return . foldl1 op $ map (\(Snum n) -> n) vals
                 Right _ -> Left $ "Bad type (expected numeric)"
                 Left err -> Left err
  in (fmap Snum ret, env')

prim_cat :: Sval
prim_cat = Sprim $ \vs e ->
  let (vs', _) = evalList vs e
      ret = do vals <- vs'
               case fst $ run (prim_apply stringp vals) e of
                 Right (Sbool True) -> return . Sstring $ concatMap (\(Sstring s) -> s) vals
                 Right _ -> Left $ "Bad type (expected strings)"
                 Left err -> Left err
  in (ret, e)

prim_add :: Sval
prim_add = fold_num (+)
prim_sub :: Sval
prim_sub = fold_num (-)
prim_mul :: Sval
prim_mul = fold_num (*)
prim_div :: Sval
prim_div = fold_num quot

prim_eq :: Sval
prim_eq = Sprim $ \vs e ->
  let (vs',e') = evalList vs e
      ret = do vals <- vs'
               return $ Sbool . and . zipWith (==) vals $ List.drop 1 vals
  in (ret,e')

prim_notify :: Sval
prim_notify = Sprim $ \vs e ->
  case evalList vs e of
    (Left err,_) -> (Left err, e)
    (Right [Sworld (f,c), Sstring s], _) -> case handle f of
      Nothing -> (Right (Sstring s), e)
      Just h -> unsafePerformIO $ do hPutStrLn h s
                                     hFlush h
                                     return (Right (Sstring s), e)
    (Right l,_) -> (Left $ "bad arguments: " ++ show l, e)


prim_if :: Sval
prim_if = Sprim $ \vs env ->
  case vs of
    [cond, y, n] -> let (v,_) = run (prim_eval cond) env
      in case v of Left err -> (Left err, env)
                   Right v' -> case v' of (Sbool False) -> run (prim_eval n) env
                                          _             -> run (prim_eval y) env


prim_name :: Sval
prim_name = Sprim $ \vs e ->
  case evalList vs e of
    (Left err, _) -> (Left err,e)
    (Right [Sworld w],_) -> (return $ Sstring (name . focus $ w), e)
    _ -> (Left "Bad type (expected world)", e)
prim_desc :: Sval
prim_desc = Sprim $ \vs e -> case vs of [Sworld w] -> (return $ Sstring (name . focus $ w), e)
                                        _ -> (Left "Bad type (expected world)", e)

-- | Function application.
-- Handles fns defined in wisp, primitive fns, and exprs that (may) evaluate
-- to fns as separate cases.
-- TODO: see if this can be shorter & add support for special forms!
prim_apply :: Sval -> [Sval] -> Expr (Either String Sval)
prim_apply (Sfunc ps body fe) vs = Expr $ \env ->
  if length ps /= length vs then (Left $ "Wrong number of arguments: " ++ show (length vs) ++ " for " ++ show (length ps), env)
    else let (vs', env') = evalList vs env
             ret = do vals <- vs'
                      let fe' = (M.fromList (ps `zip` vals)):fe -- push a new frame onto the stack
                      fst $ run (foldl1 (>>) $ map prim_eval body) fe'
         in (ret,env')
prim_apply (Sprim f) vs = Expr $ \env -> f vs env

prim_apply (Ssym s) vs = Expr $ \env ->
  case envLookup s env of
    Nothing -> (Left $ "Unable to resolve symbol: " ++ s, env)
    Just v  -> if v == Ssym s then (Left $ "Circular definition: " ++ s, env)
                              else run (prim_apply v vs) env

prim_apply (Sform ps body) vs = Expr $ \env ->
  if length ps /= length vs then (Left $ "Wrong number of arguments: " ++ show (length vs) ++ " for " ++ show (length ps), env)
    else let env' = (M.fromList (ps `zip` vs)):env
             ret = fst $ run (foldl1 (>>) $ map prim_eval body) env'
         in (ret,env')


prim_apply (Slist l) vs = Expr $ \env -> 
  let fn = fst $ run (prim_eval $ Slist l) env
      ret = do fn' <- fn
               return $ run (prim_apply fn' vs) env
  in case ret of Right v -> v
                 Left err -> (Left err, env)
prim_apply v _ = Expr $ \env -> (Left $ "Non-applicable value: " ++ show v, env)

-- | Sval evaluation.
prim_eval :: Sval -> Expr (Either String Sval)
prim_eval sv = Expr $ \env ->
  case sv of Slist (o:vs) -> run (prim_apply o vs) env
             Ssym s -> case envLookup s env of
                         Just v -> (return v, env)
                         Nothing -> (Left $ "Unable to resolve sumbol: " ++ s, env)
             _ -> (return sv, env)


envLookup :: String -> Env -> Maybe Sval
envLookup _ [] = Nothing
envLookup s (f:fs) = case M.lookup s f of Nothing -> envLookup s fs
                                          Just v -> Just v

evalList :: [Sval] -> Env -> (Either String [Sval], Env)
evalList vs env = let (vs',env') = foldl acc ([],env) $ map prim_eval vs
                      acc (l,s) m = let (r,s') = run m s in (r:l,s')
                  in (sequence (reverse vs'), env')


toplevelBindings = [frame]
  where frame = M.fromList $
          [ ("lambda", prim_lambda)
          , ("+",      prim_add   )
          , ("-",      prim_sub   )
          , ("*",      prim_mul   )
          , ("/",      prim_div   )
          , ("=",      prim_eq    )
          , ("define", prim_define)
          , ("if",     prim_if    )
          , ("notify", prim_notify)
          , ("name",   prim_name  )
          , ("cat",    prim_cat   )
          ]

{- TYPE PREDICATES -}

-- FIXME: these don't eval their arguments properly

stringp :: Sval
stringp = Sprim $ \vs e -> (return . Sbool $ all str vs, e)
  where str v = case v of Sstring _ -> True
                          _ -> False

nump :: Sval
nump = Sprim $ \vs e -> (return . Sbool $ all num vs, e)
  where num v = case v of Snum _ -> True
                          _ -> False

boolp :: Sval
boolp = Sprim $ \vs e -> (return . Sbool $ all bln vs, e)
  where bln v = case v of Sbool _ -> True
                          _ -> False

funcp :: Sval
funcp = Sprim $ \vs e -> (return . Sbool $ all fn vs, e)
  where fn v = case v of Sfunc _ _ _ -> True
                         _ -> False

worldp :: Sval
worldp = Sprim $ \vs e -> (return . Sbool $ all wd vs, e)
  where wd v = case v of Sworld _ -> True
                         _ -> False

{- PARSER -}

parseWisp = parse sexp ""

sexp = fmap Slist $ char '(' *> expr `sepBy` whitespace <* char ')'

whitespace = wsChar >> many wsChar
  where wsChar = oneOf " \n\t\r"

expr = sexp <|> atom

atom = str <|> symbol <|> number <|> true <|> false

str = Sstring `fmap` (char '"' *> many stringContents <* char '"')
  where stringContents = try (string "\\\"" >> return '"') <|> noneOf "\""

true = Sbool `fmap` (try (string "#t") >> return True)
false = Sbool `fmap` (try (string "#f") >> return False)

nonNum = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ "_+-=*/.'")

number = (Snum . read) `fmap` ((:) <$> digit <*> many digit)

symbol = Ssym `fmap` ((:) <$> nonNum <*> many (digit <|> nonNum))


