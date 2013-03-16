{-# LANGUAGE BangPatterns #-}
module WitchHouse.World.Interop
( objlevel
, invoke
, evalWisp
, notify
, notifyExcept
) where

-- SRSLY consider whether we really want to do it like this.
import System.IO.Unsafe

import WitchHouse.Types
import WitchHouse.World.Core
import WitchHouse.Wisp
import WitchHouse.Wisp.Core (envLookup, p_apply)
import Data.List (delete)

import qualified Data.Map as M

import System.IO (hPutStrLn, hFlush)

objlevel :: Env
objlevel = snd . unsafePerformIO $ run defs toplevel'
  where
    tl = fst $ toplevel M.! 0
    toplevel' = M.insert 0 (objBindings `M.union` tl, -1) toplevel
    objBindings = M.fromList $
      [ ("notify", wisp_notify)
      , ("notify-room", wisp_notify_loc)
      , ("contents", wisp_w_contents)
      , ("name", wisp_w_name)
      , ("desc", wisp_w_desc)
      , ("w-up", wisp_w_up)
      , ("w-dn", wisp_w_dn)
      , ("location", wisp_w_loc)
      ]
    defs = runWisp . unlines $
      [ "(begin"

      , "  (define (quiet-exit) (set! *self* (w-up *self*)))"

      , "  (define (enter n) (set! *self* (w-dn *self* (if (world? n) (name n) n))))"
      , "  (define (join strs j)"
      , "    (fold (lambda (s1 s2) (cat s1 j s2)) (car strs) (cdr strs)))"

      , "  (define (take w)"
      , "    (enter"
      , "      (notify-room (cat \"You take \" *name* \".\")"
      , "                   (cat (name w) \" takes \" *name* \".\")"
      , "                   w)))"
      
      , "  (define (drop w)"
      , "    (quiet-exit)"
      , "    (notify-room (cat \"You drop \" *name* \".\")"
      , "                 (cat (name w) \" drops \" *name* \".\")"
      , "                 w))"

      , "  (define (exit)"
      , "    (notify-room \"\""
      , "                 (cat *name* \" arrives.\")"
      , "                 (begin"
      , "                   (set! *self*"
      , "                         (w-up (notify-room \"\""
      , "                                            (cat *name* \" exits.\")"
      , "                                            *self*)))"
      , "                   (look))))"

      , "  (define (look)"
      , "    (notify (join (append (list (name (location *self*))"
      , "                                (desc (location *self*)))"
      , "                          (map (lambda (i) (cat (name i) \" is here.\"))"
      , "                               (contents (location *self*))))"
      , "                  \"\n\")"
      , "            *self*))"


      , "  (define (whoami)"
      , "    (notify *name* *self*))"

      , "  (define (inventory)"
      , "    ((lambda (i)"
      , "       (if (null? i)"
      , "           (notify \"You aren't carrying anything.\" *self*)"
      , "           (notify (join (cons \"You are carrying:\""
      , "                               (map name i))"
      , "                         \"\n\")"
      , "                   *self*)))"
      , "     (contents *self*)))" 
      , ")"
      ]


invoke :: String -> [Sval] -> World -> IO (Either String (Sval,World))
invoke f sv w = let (o,cs) = bindAttrs w
  in case envLookup f 0 (bindings o) of
       Nothing -> return . Left $ "Unable to resolve symbol: " ++ f
       Just fn -> do
         res <- run (p_apply fn sv 0) (bindings o)
         return $ case res of
           (Right s,env) -> Right (s, applyAttrs env (o,cs))
           (Left err,_)  -> Left err

evalWisp :: String -> World -> IO (Either String World)
evalWisp s w = let (o,cs) = bindAttrs w in do
  res <- run (runWisp s) (bindings o)
  return $ case res of
    (Right _, env) -> Right (applyAttrs env (o,cs))
    (Left err, _) -> Left err

bindAttrs :: World -> World
bindAttrs (o,cs) = let bs = M.fromList [ ("*name*", Sstring $ name o)
                                       , ("*desc*", Sstring $ description o)
                                       , ("*self*", Sworld (o,cs))
                                       ]
                       (tl,_) = bindings o M.! 0
  in (o{bindings = M.insert 0 ((bs `M.union` tl),-1) (bindings o)},cs)

applyAttrs :: Env -> World -> World
applyAttrs b w = apName . apDesc $ apWrld
  where (tl,_) = b M.! 0
        apWrld = case M.lookup "*self*" tl of { Just (Sworld (f,cs)) -> (f{bindings = b},cs) ; _ -> w }
        apName w'@(f,cs) = case M.lookup "*name*" tl of { Just (Sstring n) -> (f{name = n},cs); _ -> w' }
        apDesc w'@(f,cs) = case M.lookup "*desc*" tl of { Just (Sstring d) -> (f{description = d},cs); _ -> w' }


wisp_w_up :: Sval
wisp_w_up = Sprim $ \vs _ e -> return $ case vs of
  [Sworld w] -> (Sworld `fmap` exit w, e)
  l -> (Left $ "bad arguments: " ++ show l, e)

wisp_w_loc :: Sval
wisp_w_loc = Sprim $ \vs _ e -> return $ case vs of
  [Sworld w] -> (Sworld `fmap` zUp w, e)
  l -> (Left $ "bad arguments: " ++ show l, e)

wisp_w_dn :: Sval
wisp_w_dn = Sprim $ \vs _ e -> return $ case vs of
  [Sworld w, Sstring n] -> (Sworld `fmap` enter (matchName n) w, e)
  l -> (Left $ "bad arguments: " ++ show l, e)

wisp_w_contents :: Sval
wisp_w_contents = Sprim $ \vs _ e -> return $ case vs of
  [Sworld w] -> (Right . Slist . map Sworld . zDn $ w, e)
  l -> (Left $ "bad arguments: " ++ show l, e)

wisp_w_name :: Sval
wisp_w_name = Sprim $ \vs _ e -> return $ case vs of
  [Sworld (f,_)] -> (Right . Sstring . name $ f, e)
  l -> (Left $ "bad arguments: " ++ show l, e)

wisp_w_desc :: Sval
wisp_w_desc = Sprim $ \vs _ e -> return $ case vs of
  [Sworld (f,_)] -> (Right . Sstring . description $ f, e)
  l -> (Left $ "bad arguments: " ++ show l, e)

wisp_notify :: Sval
wisp_notify = Sprim $ \vs _ e -> case vs of
  [Sstring s, Sworld w] -> do rw <- notify s w
                              return (Right . Sworld $ rw,e)
  l -> return (Left $ "bad arguments: " ++ show l, e)

wisp_notify_loc :: Sval
wisp_notify_loc = Sprim $ \vs _ e -> case vs of
  [Sstring s, Sstring o, Sworld w] -> do rw <- notify s w >> notifyExcept o w
                                         return (Right . Sworld $ rw ,e)
  l -> return (Left $ "bad arguments: " ++ show l, e)

notify :: String -> World -> IO World
notify msg w = case handle . fst $ w of Nothing -> return w
                                        Just h -> hPutStrLn h msg >> hFlush h >> return w

notifyExcept :: String -> World -> IO World
notifyExcept msg w = case zUp w of Left _ -> return w
                                   Right w' -> mapM_ (notify msg) (delete w $ zDn w') >> return w


