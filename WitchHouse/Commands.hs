{-# LANGUAGE BangPatterns #-}
module WitchHouse.Commands
( parseCommand
, rootMap
) where

import WitchHouse.World
import WitchHouse.Wisp
import WitchHouse.Types
import Prelude hiding (take,drop)
import Data.List (isPrefixOf, intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Char
import System.IO
import Control.Monad ((>=>))


type Command = [String] -> WIO
type WIO     = World -> IO World

-- | Parse an input string into a command using the given
-- command map.
parseCommand :: Map String Command -> String -> WIO
parseCommand cm s = case tokenize s of
  Just (c:args) -> mLookup c cm args
  _ -> huh

-- | Default command map.
rootMap :: Map String Command
rootMap = M.fromList $
  [ ("take", takes)
  , ("drop", drops)
  , ("enter", enters)
  , ("exit", leaves)
  , ("go",   goes)
  , ("@make", makes)
  , ("look", looks)
  , ("quit", quit)
  , ("@link", links)
  , ("@unlink", unlinks)
  , ("say", say)
  , ("/me", me)
  , ("help", help)
  , ("whoami", whoami)
  , ("exits", listExits)
  , ("inventory", inventory)
  , ("@eval", oEval)
  ]

{- NOTIFICATION HELPERS -}

notifyResult :: (World -> Either String World) -> WIO -> WIO
notifyResult wt wio w = case wt w of Left err -> notify err w
                                     Right w' -> wio $ find' (==focus w) Global w'

huh :: WIO
huh = notify "Huh?"

{- COMMANDS -}

help :: Command
help _ = notify helpMsg
  where
    helpMsg = unlines $
      [ "A superset of these commands is available:"
      , "  look"
      , "  go        <direction>"
      , "  take      <object>"
      , "  drop      <object>"
      , "  enter     <object>"
      , "  exit"
      , "  @make     <object>"
      , "  @link     <origin> <destination> <direction>"
      , "  @unlink   <origin> <direction>"
      , "  say       [message]"
      , "  /me       [whatever it is that you do]"
      , "  help"
      , "  whoami"
      ]

whoami :: Command
whoami [] = name . focus >>= notify
whoami _ = huh

send :: Command
send [actn,t] w = case find (matchName t) (Distance 2) w of
  Left err -> notify err w
  Right w' -> case invoke actn [Sworld w] (bindings . focus $ w') of
                Left err -> notify err w
                Right (Sworld !w'') -> return w''
                Right sv -> notify ("Bad return type: " ++ show sv) w
send _ w = huh w

oEval :: Command
oEval [t,s] = notifyResult (find (matchName t) (Distance 2)  >=> evalWisp s) return
oEval _ = huh

listExits :: Command
listExits [] w = let xs = map fst . M.toList . exits . focus $ zUp' w
  in notify (intercalate "\n" $ "The following exits are available:":xs) w
listExits _ w = huh w

inventory :: Command
inventory [] w = case contents $ focus w of
  [] -> notify "You aren't carrying anything." w
  inv -> notify (intercalate "\n" $ "Inventory:":(map name inv)) w
inventory _ w = huh w

takes :: Command
takes [n] = notifyResult (take $ matchName n) $ 
            notify ("You now have "++n++".") >=> ((++" takes "++n++".") . name . focus >>= notifyExcept)
takes _ = huh

drops :: Command
drops [n] = notifyResult (\w -> drop (matchName n) w >>= find (focus w==) Global) $
            notify ("You drop "++n++".") >=> ((++" drops "++n++".") . name . focus >>= notifyExcept)
drops _ = huh

quit :: Command
quit [] (f,c) = case handle f of Nothing -> return (f,c)
                                 Just h -> do hPutStrLn h "Bye!"
                                              hClose h
                                              return (f{handle = Nothing},c)
quit _ w = huh w

goes :: Command
goes [dir] = notifyResult (go dir) $ looks [] >=> ((++" arrives.") . name . focus >>= notifyExcept)
goes _ = huh

leaves :: Command
leaves [] = notifyResult exit $ looks [] >=> ((++" enters.") . name . focus >>= notifyExcept)
leaves _ = huh

enters :: Command
enters [n] = notifyResult (enter $ matchName n) $ looks [] >=> ((++"  enters.") . name . focus >>= notifyExcept)
enters _ = huh

makes :: Command
makes [n] = make n >=> notify ("You've made "++n)
makes _ = huh

looks :: Command
looks [] = descLoc >>= notify
looks _ = huh

descLoc :: World -> String
descLoc w = let c = focus.zUp' $ w
                dv = replicate (length . name $ c) '-'
  in intercalate "\n" $ [name c, dv, description c, dv] ++ map (\o -> name o ++ " is here.") (contents c)

links :: Command
links [dir,dest] = notifyResult (\w -> zUp w >>= link dir (matchName dest) >>= find (focus w ==) Self) $
                   notify ("Linked: "++dir++" => "++dest)
links _ = huh

unlinks :: Command
unlinks [dir] = notifyResult (\w -> zUp w >>= unlink dir >>= find (focus w==) Self) (notify $ "Unlinked: "++dir)
unlinks _ = huh

say :: Command
say [] = notify "Say what?"
say m = let msg = unwords m in notify ("You say \""++msg++"\"") >=> ((++" says \""++msg++"\"") . name . focus >>= notifyExcept)

me :: Command
me [] = notify "What do you do?"
me m = let msg = unwords . (:m) . name . focus in (msg >>= notify) >=> (msg >>= notifyExcept)

{- PARSING HELPERS -}

mLookup :: String -> Map String Command -> Command
mLookup k cm = case M.lookup k cm of
  Just c -> c
  Nothing -> case filter (isPrefixOf k) (M.keys cm) of
    [m] -> cm M.! m
    [] -> \as -> send (k:as)
    ms -> \_ -> notify ("You could mean: " ++ show ms)
  

-- | TODO: Write tests for this. Make it generally suck less.
-- maybe use parsec
tokenize :: String -> Maybe [String]
tokenize s = sequence $ unquoted s []
  where
    unquoted [] [] = []
    unquoted [] a = [Just $ reverse a]
    unquoted (c:cs) a
      | isSpace c && null a = unquoted cs a
      | isSpace c = (Just $ reverse a):(unquoted cs [])
      | isQuote c && null a = quoted c cs a
      | isQuote c = (Just $ reverse a):(quoted c cs [])
      | isEscape c = escape unquoted cs a
      | otherwise = unquoted cs (c:a)

    quoted _ [] _ = [Nothing]
    quoted q (c:cs) a
      | c == q && null a = unquoted cs []
      | c == q = (Just $ reverse a):(unquoted cs [])
      | isEscape c = escape (quoted q) cs a
      | otherwise = quoted q cs (c:a)

    isQuote c = c `elem` "`'\""
    isEscape c = c == '\\'
    escape _ [] _ = [Nothing]
    escape mode (c:cs) acc = mode cs (c:acc)

