-- predicates for type & argument checking
module WitchHouse.Wisp.TC
( tc_str
, tc_num
, tc_bool
, tc_func
, tc_macro
, tc_world
, tc_sym
, tc_list
, tc_prim
, tc_form
, tc_fail
, len_fail
) where

import WitchHouse.Types

tc_str :: Sval -> Bool
tc_str s = case s of { Sstring _ -> True; _ -> False }
tc_num :: Sval -> Bool
tc_num s = case s of { Snum _ -> True; _ -> False }
tc_bool :: Sval -> Bool
tc_bool s = case s of { Sbool _ -> True; _ -> False }
tc_func :: Sval -> Bool
tc_func s = case s of { Sfunc _ _ _ -> True; _ -> False }
tc_macro :: Sval -> Bool
tc_macro s = case s of { Smacro _ _ _ -> True; _ -> False }
tc_world :: Sval -> Bool
tc_world s = case s of { Sworld _ -> True; _ -> False }


tc_sym :: Sval -> Bool
tc_sym s = case s of { Ssym _ -> True; _ -> False }
tc_list :: Sval -> Bool
tc_list s = case s of { Slist _ -> True; _ -> False }
tc_prim :: Sval -> Bool
tc_prim s = case s of { Sprim _ -> True; _ -> False }
tc_form :: Sval -> Bool
tc_form s = case s of { Sform _ -> True; _ -> False }


tc_fail :: Show a => (a -> Bool) -> a -> Either String ()
tc_fail p v = if p v then return () else Left $ "Bad type: " ++ show v

len_fail :: Int -> [a] -> Either String b
len_fail n l = Left $ "Wrong number of arguments: " ++ show (length l) ++ " for " ++ show n

