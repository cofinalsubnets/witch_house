module Gretel.Interface.Commands (storyMap, rootMap) where

import Prelude hiding (take, drop)
import Data.Maybe (fromMaybe, fromJust, isJust)
import Gretel.World
import Gretel.Interface.Types
import qualified Data.Map as M
import Data.List hiding (take, drop)

storyMap :: CommandMap
storyMap = M.fromList $
  [ ("go",        go       )
  , ("take",      take     )
  , ("look",      look     )
  , ("drop",      drop     )
  , ("examine",   examine  )
  , ("exits",     cExits   )
  , ("say",       say      )
  , ("/me",       me       )
  , ("help",      help     )
  , ("inventory", inventory)
  , ("whoami",    whoami   )
  , ("quit",      quit     )
  ]

rootMap :: CommandMap
rootMap = M.union storyMap . M.fromList $
  [ ("make",      make     )
  , ("link",      link     )
  , ("unlink",    unlink   )
  , ("enter",     enter    )
  , ("exit",      exit     )
  , ("describe",  describe )
  , ("kill",      destroy  )
  ]

notifyAll :: String -> String -> World -> IO ()
notifyAll r msg w = mapM_ (\n -> notify n msg w) $ contents r w

notifyAllBut :: String -> String -> String -> World -> IO ()
notifyAllBut n r msg w = mapM_ (\p -> notify p msg w) $ filter (n/=) $ contents r w

help :: Command
help _ n w = notify n helpMsg w >> return w
  where
    helpMsg = unlines $
      [ "A superset of these commands is available:"
      , "  look     [direction]"
      , "  exits"
      , "  go       <direction>"
      , "  take     <object>"
      , "  drop     <object>"
      , "  inventory"
      , "  enter    <object>"
      , "  exit"
      , "  examine  <object>"
      , "  make     <object>"
      , "  kill     <object>"
      , "  link     <origin> <destination> <direction>"
      , "  unlink   <origin> <direction>"
      , "  describe <object>"
      , "  say      [message]"
      , "  /me      [whatever it is that you do]"
      , "  help"
      , "  whoami"
      ]

huh :: String -> World -> IO World
huh n w = notify n "Huh?" w >> return w

quit :: Command
quit [] n w = do notify n "Bye!" w
                 let o = get' n w
                 kill . fromJust $ client o
                 return $ set o { client = Nothing } w
quit _ n w = huh n w

say :: Command
say s n w = let msg = "\"" ++ unwords s ++ "\""
  in do notify n ("You say, "++msg) w
        notifyAllBut n (getLoc n w) (n++" says, "++msg) w
        return w

me :: Command
me s n w = let msg = unwords (n:s)
  in do notifyAll (getLoc n w) msg w
        return w

destroy :: Command
destroy [t] n w
 | not $ member t w = notify n "You can't destroy what doesn't exist!" w >> return w
 | t == name (root w) = notify n "That would destroy the universe." w >> return w
 | isJust . client $ get' t w = notify n ("You can't kill "++t++".") w >> return w
 | otherwise = notify n (t ++ " has been destroyed.") w >> del t w
destroy _ n w = huh n w

cExits :: Command
cExits [] n w = let es = case exitsFor n w of Left err -> error $ "cExits: lookup failed: " ++ err
                                              Right m -> map fst . M.toList $ m
                    ms = "The following exits are available:":es
                    msg = intercalate "\n" ms
  in notify n msg w >> return w
cExits _ n w = huh n w

inventory :: Command
inventory [] n w = let cs = contents n w
  in notify n (unlines cs) w >> return w
inventory _ n w = huh n w

whoami :: Command
whoami [] n w = notify n n w >> return w
whoami _ n w = huh n w

go :: Command
go [dir] n w = case n `goes` dir $ w of
  Left err -> notify n err w >> return w
  Right w' -> do notify n (desc (getLoc n w') w' [n]) w'
                 notifyAllBut n (getLoc n w') (n++" arrives from "++ (getLoc n w) ++ ".") w'
                 notifyAllBut n (getLoc n w) (n++" goes "++dir++".") w'
                 return w'

go [] n w = notify n "Go where?" w >> return w
go _ n w = huh n w

unlink :: Command
unlink [n,dir] _ w = case n `deadends` dir $ w of
  Left err  -> notify n err w >> return w
  Right w'  -> notify n "Done." w' >> return w'
unlink _ n w = huh n w

take :: Command
take [t] n w = case n `takes` t $ w of
  Left err -> notify n err w >> return w

  Right w' -> do notify n ("You now have a " ++ t ++ ".") w'
                 notifyAllBut n (getLoc n w') (n ++ " picks up " ++ t) w'
                 return  w'

take [] n w = notify n "Take what?" w >> return w
take _ n w = huh n w

exit :: Command
exit [] n w = case n `leaves` orig $ w of
  Left err  -> notify n err w >> return w
  Right w'  -> let dest = getLoc n w'
               in do notify n (desc dest w [n]) w'
                     notifyAllBut n dest (n++" arrives from "++orig++".") w'
                     notifyAllBut n orig (n++" exits to "++dest++".") w'
                     return w'
  where orig = getLoc n w
                 
exit _ n w = huh n w

look :: Command
look [] n w = notify n (desc (getLoc n w) w [n]) w >> return w
look [dir] n w = let xs = case exitsFor n w of Left err -> error $ "look: lookup failed: " ++ err
                                               Right m -> m
                     txt = do d <- M.lookup dir xs
                              return $ desc d w []
                     msg = fromMaybe "You don't see anything in that direction." txt
  in notify n msg w >> return w
look _ n w  = huh n w

make :: Command
make [o] n w = case n `makes` o $ w of
  Left err -> notify n err w >> return w
  Right w' -> do notify n ("You've created " ++ o ++ ".") w'
                 notifyAllBut n (getLoc n w') (n++" creates "++o++".") w'
                 return w'
make _ n w = huh n w

enter :: Command
enter [o] n w = case n `enters` o $ w of
  Left err -> notify n err w >> return w
  Right w'  -> let ol = getLoc n w
                   nl = getLoc n w'
    in do notify n (desc nl w' [n]) w'
          notifyAllBut n nl (n++" enters from "++ol++".") w'
          notifyAllBut n ol (n++" enters "++o++".") w'
          return w'
enter [] n w = notify n "Enter where?" w >> return w
enter _ n w = huh n w

drop :: Command
drop [o] n w = case n `drops` o $ w of
  Left err  -> notify n err w >> return w
  Right w'  -> do notify n ("You drop " ++ o ++ ".") w'
                  notifyAllBut n (getLoc n w') (n++" drops "++o++".") w'
                  return w'
drop _ n w = huh n w

link :: Command
link [n1,n2,d] n w = case (n1 `adjoins` n2) d w of
  Left err  -> notify n err w >> return w
  Right w'  -> notify n "Done." w' >> return w'
link _ n w = huh n w

describe :: Command
describe [o,d] n w = case (d `describes` o) w of
  Left err -> notify n err w >> return w
  Right w' -> notify n "Done." w' >> return w'
describe _ n w = huh n w

examine :: Command
examine [t] n w
  | n `member` w && t `member` w = if location (get' n w) == location (get' t w)
    then notify n (desc t w []) w >> return w
    else notify n ("You see no "++t++" here.") w >> return w
  | otherwise = huh n w
examine [] n w = notify n "Examine what?" w >> return w
examine _ n w = huh n w

-- helper fns

desc :: String -> World -> [String] -> String
desc n w xs = let o = get' n w
                  cs = contents n w \\ xs
                  ps = map (++ " is here.") cs
                  dv = replicate (length $ name o) '-'
  in intercalate "\n" . delete "" $
    [ n
    , dv
    , description o
    , dv
    ] ++ ps

getLoc :: Key -> World -> String
getLoc n = fromJust . location . get' n

