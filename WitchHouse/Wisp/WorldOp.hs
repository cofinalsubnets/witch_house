module WitchHouse.Wisp.WorldOp
( worldOps
) where

-- SRSLY consider whether we really want to do it like this.
import System.IO.Unsafe

import WitchHouse.Types
import WitchHouse.World.Notify
import WitchHouse.World.Core

import qualified Data.Map as M


worldOps :: M.Map String Sval
worldOps = M.fromList $
  [ ("notify", wisp_notify)
  , ("notify-room", wisp_notify_loc)
  , ("contents", wisp_w_contents)
  , ("name", wisp_w_name)
  ]

wisp_w_contents :: Sval
wisp_w_contents = Sprim $ \vs _ e -> case vs of
  [Sworld w] -> (Right . Slist . map Sworld . zDn $ w, e)
  l -> (Left $ "bad arguments: " ++ show l, e)

wisp_w_name :: Sval
wisp_w_name = Sprim $ \vs _ e -> case vs of
  [Sworld (f,_)] -> (Right . Sstring . name $ f, e)
  l -> (Left $ "bad arguments: " ++ show l, e)

wisp_notify :: Sval
wisp_notify = Sprim $ \vs _ e -> case vs of
  [Sstring s, Sworld w] -> (Right . Sworld . unsafePerformIO $ notify s w, e)
  l -> (Left $ "bad arguments: " ++ show l, e)

wisp_notify_loc :: Sval
wisp_notify_loc = Sprim $ \vs _ e -> case vs of
  [Sstring s, Sstring o, Sworld w] -> (Right . Sworld . unsafePerformIO $ notify s w >> notifyExcept o w, e)
  l -> (Left $ "bad arguments: " ++ show l, e)

