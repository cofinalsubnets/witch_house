module Gretel.Command.Commands
( rootMap
, huh
) where

import Prelude hiding (take, drop)
import Gretel.World
import Gretel.Command.Types
import Data.Maybe
import System.IO.Unsafe
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (intercalate)
import System.Exit

rootMap = M.fromList $
  [ ("go", go)
  , ("take", take)
  , ("look", look)
  , ("make", make)
  , ("drop", drop)
  , ("link", link)
  , ("enter", enter)
  , ("exit", exit)
  , ("describe", describe)
  , ("examine", examine)
  ]

go :: Command String
go n [dir] w = case n `goes` dir $ w of
  (False,w') -> ("You can't go that way!",w')
  (True, w') -> look n [] w'
go _ [] w = ("Go where?",w)
go _ _ w = huh w

take :: Command String
take n [t] w = case n `takes` t $ w of
  (False,w') -> ("There's no " ++ t ++ " here.",w')
  (True, w') -> ("You now have a " ++ t ++ ".",w')
take _ [] w = ("Take what?",w)

exit :: Command String
exit n [] w = case n `leaves` (name $ locOf n w) $ w of
  (False,w') -> ("You can't exit your current location.",w')
  (True,w')  -> look n [] w'
exit _ _ w = huh w

huh :: WorldTransformer String
huh w = ("Huh?",w)

look :: Command String
look n [] w = (desc (locOf n w) [n],w)
look _ _ w  = huh w

make :: Command String
make n [o] w = case n `makes` o $ w of
  (False,w')  -> (o ++ " already exists!",w')
  (True,w') -> ("You've created " ++ o ++ "!",w')
make _ _ w = huh w

enter :: Command String
enter n [o] w = case n `enters` o $ w of
  (False,w') -> ("You can't enter "++o++".",w')
  (True,w')  -> look n [] w'
enter _ [] w = ("Enter where?",w)
enter _ _ w = huh w

drop :: Command String
drop n [o] w = case n `drops` o $ w of
  (False,w') -> ("You can't drop what you don't have!",w')
  (True,w')  -> ("You drop " ++ o ++ ".",w')
drop _ _ w = huh w

link :: Command String
link n [n1,n2,d] w = case (n1 `adjoins` n2) d w of
  (False,w') -> ("You can't link those rooms!",w')
  (True,w')  -> (n1 ++ " now adjoins " ++ n2,w')
link _ _ w = huh w

describe :: Command String
describe n [o,d] w = case (d `describes` o) w of
  (False,w') -> huh w'
  (True,w')  -> ("Ok.",w')
describe _ _ w = huh w

examine :: Command String
examine n [t] w = case find2 n t w of
  Nothing -> huh w
  Just (n1,n2) -> if location n1 == location n2
                  then (desc n2 [],w)
                  else ("You see no "++t++" here.",w)
examine _ [] w = ("Examine what?",w)
examine _ _ w = huh w

-- helper fns

locOf :: Name -> World -> Node
locOf n w = let n' = w M.! n in  w M.! (fromJust $ location n')

desc n xs = let cs = S.toList $ contents n S.\\ S.fromList xs
  in intercalate "\n" $
    [ name n
    , replicate (length $ name n) '-'
    , description n
    ] ++ map (++" is here.") cs

