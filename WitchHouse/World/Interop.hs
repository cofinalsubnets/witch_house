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
import WitchHouse.Wisp.Core (eval, pure)
import WitchHouse.Wisp.Predicates
import Data.List (delete)
import Prelude hiding (lookup)

import qualified Data.Map as M
import System.IO (hPutStrLn, hFlush)
import Data.ByteString.Char8 (pack)

worldLib :: Module
worldLib = (prims, defs)
  where
    prims =
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
      [ "(do"

      , "  (dfn take (w)"
      , "    (enter"
      , "      (tell-room (str \"You take \" *name* \".\")"
      , "                   (str (name w) \" takes \" *name* \".\")"
      , "                   w)))"
      
      , "  (dfn drop (w)"
      , "    (tell-room (exit w)"
      , "               (str \"You drop \" *name* \".\")"
      , "               (str (name w) \" drops \" *name* \".\")))"

      , "  (dfn exit (p)"
      , "    (let ((new (w-up p)))"
      , "      (tell-room p \"\" (str (name p) \" leaves.\"))"
      , "      (tell-room new \"\" (str (name p) \" arrives.\"))"
      , "      (look new)))"

      , "  (dfn look (p)"
      , "    (tell p (join (append (list (name (location p))"
      , "                                (desc (location p)))"
      , "                          (map (fn (i) (str (name i) \" is here.\"))"
      , "                               (filter (fn (t) (not (= t p)))"
      , "                                       (contents (location p)))))"
      , "                  \"\n\")))"


      , "  (dfn whoami (w)"
      , "    (tell w (name w)))"

      , "  (dfn inventory (me)"
      , "    (let ((cs (contents me)))"
      , "      (if (null? cs)"
      , "          (tell me \"You aren't carrying anything.\")"
      , "          (tell me (join (cons \"You are carrying:\""
      , "                              (map name cs))"
      , "                           \"\n\")))))"

      , "  (dfn exits (me)"
      , "    (let ((xs (loc-exits (location me))))"
      , "      (if (null? xs)"
      , "          (tell me \"There are no exits from your current location.\")"
      , "          (tell me (join (cons \"The following exits are available: \""
      , "                                 xs)"
      , "                           \"\n\")))))"

      
      , "  (dfn examine (w)"
      , "    (tell w *desc*))"

      , ")"
      ]


invoke :: String -> [Val] -> World -> IO (Either String Val)
invoke f sv (Obj{objId = i},_) = do
  lu <- lookup (pack f) i
  case lu of
    Left _ -> return . Left $ "I don't know what " ++ f ++ " means."
    Right fn -> eval (Lst (fn:sv)) i

evalOn :: String -> World -> IO (Either String Val)
evalOn s (Obj{objId = i},_) = evalWisp i s

wisp_w_up = Primitive
          $ taking (Exactly 1 worlds)
          $ \[Wd w] _ -> return $ Wd `fmap` exit w

wisp_w_loc = Primitive
           $ taking (Exactly 1 worlds)
           $ \[Wd w] _ -> return $ Wd `fmap` zUp w

wisp_w_dn = Primitive
          $ taking (Exactly 2 [world, string])
          $ \[Wd w, Str n] _ -> return $ Wd `fmap` enter (matchName n) w

wisp_w_contents = Primitive
                $ taking (Exactly 1 worlds)
                $ \[Wd w] _ -> return $ Right . Lst . map Wd . zDn $ w

wisp_w_desc = Primitive
            $ taking (Exactly 1 worlds)
            $ \[Wd (f,_)] _ -> return $ Right . Str . description $ f

wisp_tell = Primitive $ \vs _ -> case vs of
  [Wd w, Str s] -> notify s w >>= return . Right . Wd
  [Prt h, Str s] -> do hPutStrLn h s
                       hFlush h
                       return . Right $ Str s
  l -> return . Left $ "bad arguments: " ++ show l

wisp_tell_loc = Primitive
              $ taking (Exactly 3 [world, string, string])
              $ \[Wd w, Str s, Str o] _ -> notify s w >>= notifyExcept o >>= return . return . Wd

wisp_exits = Primitive
           $ taking (Exactly 1 worlds)
           $ pure
           $ \[Wd (o,_)] _ -> Lst . map Str . M.keys $ exits o

wisp_go = Primitive
        $ taking (Exactly 2 [string, world])
        $ \[Str d, Wd w] _ -> return $ Wd `fmap` go d w

wisp_neighbour = Primitive
               $ taking (Exactly 2 [string, world])
               $ \[Str n, Wd w] _ -> return $ Wd `fmap` find (matchName n) Location w

wisp_w_name = Primitive
            $ taking (Exactly 1 worlds)
            $ pure
            $ \[Wd (f,_)] _ -> Str $ name f

{- haskell-level notification primitives -}

notify :: String -> World -> IO World
notify msg w = case handle . fst $ w of
  Nothing -> return w
  Just h -> hPutStrLn h msg >> hFlush h >> return w

notifyExcept :: String -> World -> IO World
notifyExcept msg w = case zUp w of
  Left _ -> return w
  Right w' -> mapM_ (notify msg) (delete w $ zDn w') >> return w

