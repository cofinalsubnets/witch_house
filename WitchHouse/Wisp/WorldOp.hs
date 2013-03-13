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
  [ ("notify", wisp_notify)
  , ("notify-room", wisp_notify_loc)
  ]

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

