{-# LANGUAGE TupleSections #-}
module WitchHouse.World.Interop
( worldLib
, invoke
, evalOn
, notify
, notifyExcept
) where


import WitchHouse.Types
import WitchHouse.World.Core
import WitchHouse.Wisp
import WitchHouse.Wisp.Core (eval)
import WitchHouse.Wisp.Predicates
import Data.List (delete)
import Prelude hiding (lookup)

import qualified Data.Map as M
import System.IO (hPutStrLn, hFlush)
import Data.ByteString.Char8 (pack)

worldLib :: Module
worldLib = (primitives, defs)
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
  lu <- lookup (pack f) i
  case lu of
    Left _ -> return . Left $ "I don't know what " ++ f ++ " means."
    Right fn -> eval (Slist (fn:sv)) i

evalOn :: String -> World -> IO (Either String Sval)
evalOn s (Obj{objId = i},_) = evalWisp i s

wisp_w_up = guard' (Exactly 1) [worldP] $ \[Sworld w] _ ->
  return $ Sworld `fmap` exit w

wisp_w_loc = guard' (Exactly 1) [worldP] $ \[Sworld w] _ ->
  return $ Sworld `fmap` zUp w

wisp_w_dn = guard' (Exactly 2) [worldP, strP] $ \[Sworld w, Sstring n] _ ->
  return $ Sworld `fmap` enter (matchName n) w

wisp_w_contents = guard' (Exactly 1) [worldP] $ \[Sworld w] _ ->
  return $ Right . Slist . map Sworld . zDn $ w

wisp_w_desc = guard' (Exactly 1) [worldP] $ \[Sworld (f,_)] _ ->
  return $ Right . Sstring . description $ f

wisp_tell = Sprim $ \vs _ -> case vs of
  [Sworld w, Sstring s] -> do rw <- notify s w
                              return . Right . Sworld $ rw
  [Shandle h, Sstring s] -> do hPutStrLn h s
                               hFlush h
                               return . Right $ Sstring s
  l -> return . Left $ "bad arguments: " ++ show l

wisp_tell_loc = guard' (Exactly 3) [worldP, strP, strP] $
  \[Sworld w, Sstring s, Sstring o] _ -> do
    notify s w >>= notifyExcept o >>= return . Right . Sworld

wisp_exits = guard' (Exactly 1) [worldP] $ \[Sworld (o,_)] _ ->
  return . Right . Slist . map Sstring . M.keys $ exits o

wisp_go = guard' (Exactly 2) [strP, worldP] $ \[Sstring d, Sworld w] _ -> 
  return $ Sworld `fmap` go d w

wisp_neighbour = guard' (Exactly 2) [strP, worldP] $ \[Sstring n, Sworld w] _ ->
  return $ Sworld `fmap` find (matchName n) Location w

wisp_w_name = guard' (Exactly 1) [worldP] $ \[Sworld (f,_)] _ ->
  return $  Right . Sstring . name $ f

{- haskell-level notification primitives -}

notify :: String -> World -> IO World
notify msg w = case handle . fst $ w of
  Nothing -> return w
  Just h -> hPutStrLn h msg >> hFlush h >> return w

notifyExcept :: String -> World -> IO World
notifyExcept msg w = case zUp w of
  Left _ -> return w
  Right w' -> mapM_ (notify msg) (delete w $ zDn w') >> return w

