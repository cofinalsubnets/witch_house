module WitchHouse.Wisp.Predicates
( tc
, lc
, some
, noop
, tc_num
, tc_str
, tc_sym
, tc_bool
, tc_world
, tc_ref
, tc_handle
, tc_list
, tc_prim
, tc_macro
, tc_func
) where

import WitchHouse.Types

tc_str (Sstring _) = True
tc_str _ = False

tc_num (Sfixn _)  = True
tc_num (Sfloat _) = True
tc_num _ = False

tc_bool (Sbool _) = True
tc_bool _ = False

tc_func Sfunc{} = True
tc_func _ = False

tc_macro Smacro{} = True
tc_macro _ = False

tc_world (Sworld _) = True
tc_world _ = False

tc_ref (Sref _) = True
tc_ref _ = False

tc_handle (Shandle _) = True
tc_handle _ = False

tc_sym (Ssym _) = True
tc_sym _ = False

tc_list (Slist _) = True
tc_list _ = False

tc_prim (Sprim _) = True
tc_prim _ = False

some fn [] = const . return $ Left "ERROR: no arguments given"
some fn vs = fn vs

tc ts fn vs
  | and $ zipWith ($) ts vs = fn vs
  | otherwise = const $ return typeError
  where typeError = Left $ "ERROR: bad type: " ++ show (Slist vs)

noop = const True

lc n fn vs
  | length vs == n = fn vs 
  | otherwise = const $ return argError
  where argError = Left $ "ERROR: wrong number of arguments: " ++ show (length vs) ++ " for " ++ show n

