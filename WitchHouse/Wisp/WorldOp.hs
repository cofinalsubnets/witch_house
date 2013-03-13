module WitchHouse.Wisp.WorldOp
( worldOps
) where

-- SRSLY consider whether we really want to do it like this.
import System.IO.Unsafe

import WitchHouse.Types
import WitchHouse.World.Notify
import WitchHouse.Wisp.Core

import qualified Data.Map as M


worldOps :: M.Map String Sval
worldOps = M.fromList $
  [ ("name", get_name)
  , ("set-name", set_name)
  , ("desc", get_desc)
  , ("notify", wisp_notify)
  , ("notify-room", wisp_notify_loc)
  ]

set_name :: Sval
set_name = Sprim $ \vs f e ->
  case evalList vs f e of
    (Left err,_) -> (Left err,e)
    (Right [Sstring nm, Sworld (fo,ct)], _) -> (Right $ Sworld (fo{name=nm},ct), e)
    _ -> (Left "Type error: wanted <string> <world>", e)

get_name :: Sval
get_name = Sprim $ \vs f e ->
  case evalList vs f e of
    (Left err, _) -> (Left err,e)
    (Right [Sworld w],_) -> (return $ Sstring (name . fst $ w), e)
    _ -> (Left "Bad type (expected world)", e)

get_desc :: Sval
get_desc = Sprim $ \vs f e ->
  case evalList vs f e of
    (Left err, _) -> (Left err,e)
    (Right [Sworld w],_) -> (return $ Sstring (name . fst $ w), e)
    _ -> (Left "Bad type (expected world)", e)

wisp_notify :: Sval
wisp_notify = Sprim $ \vs f e ->
  case evalList vs f e of
    (Left err,_) -> (Left err, e)
    (Right [Sstring s, Sworld w], _) -> (Right . Sworld . unsafePerformIO $ notify s w, e)
    (Right l,_) -> (Left $ "bad arguments: " ++ show l, e)

wisp_notify_loc :: Sval
wisp_notify_loc = Sprim $ \vs f e ->
  case evalList vs f e of
    (Left err,_) -> (Left err, e)
    (Right [Sstring s, Sstring o, Sworld w], _) -> (Right . Sworld . unsafePerformIO $ notify s w >> notifyExcept o w, e)
    (Right l,_) -> (Left $ "bad arguments: " ++ show l, e)

