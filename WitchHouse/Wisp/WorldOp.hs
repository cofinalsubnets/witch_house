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
  , ("desc", wisp_w_desc)
  , ("w-up", wisp_w_up)
  , ("w-dn", wisp_w_dn)
  , ("location", wisp_w_loc)
  ]

wisp_w_up :: Sval
wisp_w_up = Sprim $ \vs _ e -> case vs of
  [Sworld w] -> (Sworld `fmap` exit w, e)
  l -> (Left $ "bad arguments: " ++ show l, e)

wisp_w_loc :: Sval
wisp_w_loc = Sprim $ \vs _ e -> case vs of
  [Sworld w] -> (Sworld `fmap` zUp w, e)
  l -> (Left $ "bad arguments: " ++ show l, e)

wisp_w_dn :: Sval
wisp_w_dn = Sprim $ \vs _ e -> case vs of
  [Sworld w, Sstring n] -> (Sworld `fmap` enter (matchName n) w, e)
  l -> (Left $ "bad arguments: " ++ show l, e)

wisp_w_contents :: Sval
wisp_w_contents = Sprim $ \vs _ e -> case vs of
  [Sworld w] -> (Right . Slist . map Sworld . zDn $ w, e)
  l -> (Left $ "bad arguments: " ++ show l, e)

wisp_w_name :: Sval
wisp_w_name = Sprim $ \vs _ e -> case vs of
  [Sworld (f,_)] -> (Right . Sstring . name $ f, e)
  l -> (Left $ "bad arguments: " ++ show l, e)

wisp_w_desc :: Sval
wisp_w_desc = Sprim $ \vs _ e -> case vs of
  [Sworld (f,_)] -> (Right . Sstring . description $ f, e)
  l -> (Left $ "bad arguments: " ++ show l, e)

wisp_notify :: Sval
wisp_notify = Sprim $ \vs _ e -> case vs of
  [Sstring s, Sworld w] -> let rw = unsafePerformIO $ notify s w
                           in rw `seq` (Right . Sworld $ rw, e)
  l -> (Left $ "bad arguments: " ++ show l, e)

wisp_notify_loc :: Sval
wisp_notify_loc = Sprim $ \vs _ e -> case vs of
  [Sstring s, Sstring o, Sworld w] -> let rw = unsafePerformIO $ notify s w >> notifyExcept o w
                                      in rw `seq` (Right . Sworld $ rw , e)
  l -> (Left $ "bad arguments: " ++ show l, e)

