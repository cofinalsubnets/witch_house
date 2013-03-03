{-# LANGUAGE TupleSections #-}
module Gretel.Interface.Commands
( rootMap
, huh
) where

import Prelude hiding (take, drop)
import Gretel.World
import Gretel.Interface.Types
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (intercalate, delete)

rootMap :: CommandMap
rootMap = M.fromList $
  [ ("go", go)
  , ("take", take)
  , ("look", look)
  , ("make", make)
  , ("drop", drop)
  , ("link", link)
  , ("unlink", unlink)
  , ("enter", enter)
  , ("exit", exit)
  , ("describe", describe)
  , ("examine", examine)
  , ("exits", exits)
  , ("/say", say)
  , ("/me", me)
  ]

notifyAll :: Node -> String -> [Notification]
notifyAll r msg = map (\n -> Notify n msg) . S.toList $ contents r

notify1 :: String -> String -> [Notification]
notify1 n m = [Notify n m]

notifyAllBut :: String -> Node -> String -> [Notification]
notifyAllBut n r msg = map (\p -> Notify p msg) . filter (n/=) . S.toList $ contents r

huh :: Name -> WorldTransformer [Notification]
huh n = (notify1 n "Huh?",)

say :: Command
say n s w = let msg = n ++ ": " ++ unwords s
  in (notifyAll (locOf n w) msg,w)

me :: Command
me n s w = let msg = n ++ " " ++ unwords s
  in (notifyAll (locOf n w) msg,w)


exits :: Command
exits n [] w = let es = M.keys . edges $ locOf n w
                   ms = "The following exits are available:":es
                   msg = intercalate "\n" ms
  in (notify1 n msg,w)
exits n _ w = huh n w

go :: Command
go n [dir] w = case n `goes` dir $ w of
  (False,w') -> (notify1 n "You can't go that way!",w')
  (True, w') -> ( notify1 n (desc (locOf n w) [n]) ++ 
                  notifyAllBut n (locOf n w') (n++" arrives from "++ name (locOf n w) ++ ".") ++ 
                  notifyAll (locOf n w) (n++" goes "++dir++".")
                , w')
go n [] w = (notify1 n "Go where?",w)
go n _ w = huh n w

unlink :: Command
unlink _ [n,dir] w = case n `deadends` dir $ w of
  (False,w') -> huh n w'
  (True,w')  -> ([Notify n ""],w')
unlink n _ w = huh n w

take :: Command
take n [t] w = case n `takes` t $ w of
  (False,w') -> (notify1 n $ "There's no " ++ t ++ " here."
                , w')
  (True, w') -> ((Notify n $ "You now have a " ++ t ++ "."):(notifyAllBut n (locOf n w') $ n ++ " picks up " ++ t)
                , w')
take n [] w = ([Notify n $ "Take what?"],w)
take n _ w = huh n w

exit :: Command
exit n [] w = case n `leaves` (name orig) $ w of
  (False,w') -> (notify1 n $ "You can't exit your current location.",w')
  (True,w')  -> let dest = locOf n w'
                in ( notify1 n (desc dest [n]) ++
                     notifyAllBut n dest (n++" arrives from "++name orig) ++
                     notifyAll orig (n++" exits to "++name dest)
                   , w')
  where orig = locOf n w
                 
exit n _ w = huh n w

look :: Command
look n [] w = (notify1 n $ desc (locOf n w) [n],w)
look n [dir] w = let loc = locOf n w
                     txt = do d <- dir `from` loc
                              dest <- M.lookup d w
                              return $ desc dest []
  in case txt of
    Nothing -> (notify1 n "You don't see anything in that direction."
               , w)
    Just d  -> (notify1 n d, w)
look n _ w  = huh n w

make :: Command
make n [o] w = case n `makes` o $ w of
  (False,w')  -> (notify1 n $ o ++ " already exists!", w')
  (True,w') -> ((Notify n $ "You've created " ++ o ++ "."):(notifyAllBut n (locOf n w) $ n++" creates "++o++".")
               , w')
make n _ w = huh n w

enter :: Command
enter n [o] w = case n `enters` o $ w of
  (False,w') -> (notify1 n $ "You can't enter "++o++"."
                , w')
  (True,w')  -> let ol = locOf n w
                    nl = locOf n w'
    in ( notify1 n (desc nl [n]) ++
         notifyAllBut n nl (n++" enters from "++name ol) ++
         notifyAllBut n ol (n++" enters "++o)
       , w')
enter n [] w = (notify1 n "Enter where?",w)
enter n _ w = huh n w

drop :: Command
drop n [o] w = case n `drops` o $ w of
  (False,w') -> (notify1 n "You can't drop what you don't have!",w')
  (True,w')  -> ( notify1 n ("You drop " ++ o ++ ".") ++
                  notifyAllBut n (locOf n w') (n++" drops "++o++".")
                , w')
drop n _ w = huh n w

link :: Command
link n [n1,n2,d] w = case (n1 `adjoins` n2) d w of
  (False,w') -> (notify1 n "You can't link those rooms!",w')
  (True,w')  -> ([],w')
link n _ w = huh n w

describe :: Command
describe n [o,d] w = case (d `describes` o) w of
  (False,w') -> huh n w'
  (True,w')  -> ([],w')
describe n _ w = huh n w

examine :: Command
examine n [t] w = case find2 n t w of
  Nothing -> huh n w
  Just (n1,n2) -> if location n1 == location n2
                  then (notify1 n $ desc n2 [],w)
                  else (notify1 n $ "You see no "++t++" here.",w)
examine n [] w = (notify1 n "Examine what?",w)
examine n _ w = huh n w

-- helper fns

locOf :: Name -> World -> Node
locOf n w = let n' = w M.! n in  w M.! (fromJust $ location n')

desc :: Node -> [Name] -> String
desc n xs = let cs = S.toList $ contents n S.\\ S.fromList xs
                ps = map (++ " is here.") cs
                dv = replicate (length $ name n) '-'
  in intercalate "\n" . delete "" $
    [ name n
    , dv
    , description n
    , dv
    ] ++ ps

