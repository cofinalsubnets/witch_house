module Gretel.Command.Commands
( rootMap
, huh
) where

import Prelude hiding (take, drop)
import Gretel.Types
import Gretel.World
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (intercalate)

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
  ]

go :: Command
go n [dir] w = case n `goes` dir $ w of
  (False,w') -> (return "You can't go that way!",w')
  (True, w') -> look n [] w'
go _ [] w = (return "Go where?",w)
go _ _ w = huh w

take :: Command
take n [t] w = case n `takes` t $ w of
  (False,w') -> (return $ "There's no " ++ t ++ " here.",w')
  (True, w') -> (return $ "You now have a " ++ t ++ ".",w')
take _ [] w = (return "Take what?",w)

exit :: Command
exit n [] w = case n `leaves` (name $ locOf n w) $ w of
  (False,w') -> (return "You can't exit your current location.",w')
  (True,w')  -> look n [] w'
exit _ _ w = huh w

huh :: WorldTransformer (IO String)
huh w = (return $ "Huh?",w)

look :: Command
look n [] w = (return $ desc (locOf n w) [n],w)
look _ _ w  = huh w

make :: Command
make n [o] w = case n `makes` o $ w of
  (False,w')  -> (return $ o ++ " already exists!",w')
  (True,w') -> (return $ "You've created a " ++ o ++ "!",w')
make _ _ w = huh w

enter :: Command
enter n [o] w = case n `enters` o $ w of
  (False,w') -> (return $ "You can't enter "++o++".",w')
  (True,w')  -> look n [] w'
enter _ _ w = huh w

drop :: Command
drop n [o] w = case n `drops` o $ w of
  (False,w') -> (return $ "You can't drop what you don't have!",w')
  (True,w')  -> (return $ "You drop a " ++ o ++ ".",w')
drop _ _ w = huh w

link :: Command
link n [n1,n2,d] w = case (n1 `adjoins` n2) d w of
  (False,w') -> (return $ "You can't link those rooms!",w')
  (True,w')  -> (return $ n1 ++ " now adjoins " ++ n2,w')
link _ _ w = huh w

describe :: Command
describe n [o,d] w = case (n `describes` o) d w of
  (False,w') -> huh w'
  (True,w')  -> (return $ "Ok.",w')
describe _ _ w = huh w

-- helper fns

locOf :: Name -> World -> Node
locOf n w = let n' = fromJust $ M.lookup n w in fromJust $ M.lookup (fromJust $ location n') w

desc n xs = let cs = S.toList $ contents n S.\\ S.fromList xs
  in intercalate "\n" $
    [ name n
    , replicate (length $ name n) '-'
    , description n
    ] ++ map (++" is here.") cs

