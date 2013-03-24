{-# LANGUAGE TupleSections #-}
module WitchHouse.World.Interop
( bootstrap
, invoke
, evalOn
, notify
, notifyExcept
) where


import WitchHouse.Types
import WitchHouse.World.Core
import WitchHouse.Wisp
import WitchHouse.Wisp.Core (p_apply, p_eval)
import Data.List (delete)

import qualified Data.Map as M
import System.IO (hPutStrLn, hFlush)
import Data.ByteString.Char8 (pack)

bootstrap :: IO ()
bootstrap = do
  mapM_ (\(k,v) -> bind toplevel k v) primitives
  eval defs toplevel
  return ()

  where
    primitives =
      [ (pack "tell", wisp_tell)
      , (pack "tell-room", wisp_tell_loc)
      , (pack "contents", wisp_w_contents)
      , (pack "neighbour", wisp_neighbour)
      , (pack "name", wisp_w_name)
      , (pack "desc", wisp_w_desc)
      , (pack "w-up", wisp_w_up)
      , (pack "w-dn", wisp_w_dn)
      , (pack "location", wisp_w_loc)
      , (pack "loc-exits", wisp_exits)
      , (pack "go-dir", wisp_go)
      ]

    defs = unlines $
      [ "(begin"

      , "  (define (take w)"
      , "    (enter"
      , "      (tell-room (cat \"You take \" *name* \".\")"
      , "                   (cat (name w) \" takes \" *name* \".\")"
      , "                   w)))"
      
      , "  (define (drop w)"
      , "    (tell-room (exit w)"
      , "               (cat \"You drop \" *name* \".\")"
      , "               (cat (name w) \" drops \" *name* \".\")))"

      , "  (define (exit p)"
      , "    (let ((new (w-up p)))"
      , "      (tell-room p \"\" (cat (name p) \" leaves.\"))"
      , "      (tell-room new \"\" (cat (name p) \" arrives.\"))"
      , "      (look new)))"

      , "  (define (look p)"
      , "    (tell p (join (append (list (name (location p))"
      , "                                (desc (location p)))"
      , "                          (map (lambda (i) (cat (name i) \" is here.\"))"
      , "                               (filter (lambda (t) (not (= t p)))"
      , "                                       (contents (location p)))))"
      , "                  \"\n\")))"


      , "  (define (whoami w)"
      , "    (tell w (name w)))"

      , "  (define (inventory me)"
      , "    (let ((cs (contents me)))"
      , "      (if (null? cs)"
      , "          (tell me \"You aren't carrying anything.\")"
      , "          (tell me (join (cons \"You are carrying:\""
      , "                              (map name cs))"
      , "                           \"\n\")))))"

      , "  (define (exits me)"
      , "    (let ((xs (loc-exits (location me))))"
      , "      (if (null? xs)"
      , "          (tell me \"There are no exits from your current location.\")"
      , "          (tell me (join (cons \"The following exits are available: \""
      , "                                 xs)"
      , "                           \"\n\")))))"

      
      , "  (define (examine w)"
      , "    (tell w *desc*))"

      , ")"
      ]


invoke :: String -> [Sval] -> World -> IO (Either String Sval)
invoke f sv (Obj{objId = i},_) = do
  lu <- envLookup (pack f) (Just i)
  case lu of
    Left _ -> return . Left $ "I don't know what " ++ f ++ " means."
    Right fn -> p_apply p_eval [Slist (fn:sv)] i

evalOn :: String -> World -> IO (Either String Sval)
evalOn s (Obj{objId = i},_) = eval s i

wisp_w_up :: Sval
wisp_w_up = Sprim $ \vs _ -> return $ case vs of
  [Sworld w] -> Sworld `fmap` exit w
  l -> Left $ "bad arguments: " ++ show l

wisp_w_loc :: Sval
wisp_w_loc = Sprim $ \vs _ -> return $ case vs of
  [Sworld w] -> Sworld `fmap` zUp w
  l -> Left $ "bad arguments: " ++ show l

wisp_w_dn :: Sval
wisp_w_dn = Sprim $ \vs _ -> return $ case vs of
  [Sworld w, Sstring n] -> Sworld `fmap` enter (matchName n) w
  l -> Left $ "bad arguments: " ++ show l

wisp_w_contents :: Sval
wisp_w_contents = Sprim $ \vs _ -> return $ case vs of
  [Sworld w] -> Right . Slist . map Sworld . zDn $ w
  l -> Left $ "bad arguments: " ++ show l

wisp_w_desc :: Sval
wisp_w_desc = Sprim $ \vs _ -> return $ case vs of
  [Sworld (f,_)] -> Right . Sstring . description $ f
  l -> Left $ "bad arguments: " ++ show l

wisp_tell :: Sval
wisp_tell = Sprim $ \vs _ -> case vs of
  [Sworld w, Sstring s] -> do rw <- notify s w
                              return . Right . Sworld $ rw
  [Shandle h, Sstring s] -> do hPutStrLn h s
                               hFlush h
                               return . Right $ Sstring s
  l -> return . Left $ "bad arguments: " ++ show l

wisp_tell_loc :: Sval
wisp_tell_loc = Sprim $ \vs _ -> case vs of
  [Sworld w, Sstring s, Sstring o] -> do rw <- notify s w >> notifyExcept o w
                                         return . Right . Sworld $ rw
  l -> return . Left $ "bad arguments: " ++ show l

wisp_exits :: Sval
wisp_exits = Sprim $ \vs _ -> case vs of
  [Sworld (o,_)] -> return $ Right . Slist . map Sstring . M.keys $ exits o
  l -> return $ Left $ "bad arguments: " ++ show l

wisp_go :: Sval
wisp_go = Sprim $ \vs _ -> return $ case vs of
  [Sstring dir, Sworld w] -> Sworld `fmap` go dir w
  _ -> Left $ "bad arguments: " ++ show vs

wisp_neighbour :: Sval
wisp_neighbour = Sprim $ \vs _ -> return $ case vs of
  [Sstring n, Sworld w] -> Sworld `fmap` find (matchName n) Location w
  _ -> Left $ "bad arguments: " ++ show vs

wisp_w_name :: Sval
wisp_w_name = Sprim $ \vs _ -> return $ case vs of
  [Sworld (f,_)] -> Right . Sstring . name $ f
  l -> Left $ "bad arguments: " ++ show l

{- haskell-level notification primitives -}

notify :: String -> World -> IO World
notify msg w = case handle . fst $ w of Nothing -> return w
                                        Just h -> hPutStrLn h msg >> hFlush h >> return w

notifyExcept :: String -> World -> IO World
notifyExcept msg w = case zUp w of Left _ -> return w
                                   Right w' -> mapM_ (notify msg) (delete w $ zDn w') >> return w

