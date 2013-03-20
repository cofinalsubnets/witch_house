module WitchHouse.World.Interop
( objlevel
, invoke
, evalWisp
, notify
, notifyExcept
) where


import WitchHouse.Types
import WitchHouse.World.Core
import WitchHouse.Wisp
import WitchHouse.Wisp.Core (envLookup, p_apply)
import Data.List (delete)

import qualified Data.Map as M

import System.IO (hPutStrLn, hFlush)
import System.IO.Unsafe (unsafePerformIO)

import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString)

-- wisp is in IO but this is actually pure when run against
-- the default toplevel.
objlevel :: Env
objlevel = snd . unsafePerformIO $ run defs toplevel'
  where
    tl = fst $ toplevel M.! 0
    toplevel' = M.insert 0 (objBindings `M.union` tl, -1) toplevel
    objBindings = M.fromList $
      [ (pack "notify", wisp_notify)
      , (pack "notify-room", wisp_notify_loc)
      , (pack "contents", wisp_w_contents)
      , (pack "name", wisp_w_name)
      , (pack "desc", wisp_w_desc)
      , (pack "w-up", wisp_w_up)
      , (pack "w-dn", wisp_w_dn)
      , (pack "location", wisp_w_loc)
      , (pack "find", wisp_find)
      , (pack "loc-exits", wisp_exits)
      , (pack "go-dir", wisp_go)
      ]

    defs = runWisp . unlines $
      [ "(begin"

      , "  (define (quiet-exit)"
      , "    (set! *self* (w-up *self*)))"

      , "  (define (enter n)"
      , "    ((lambda (dest)"
      , "       (notify-room \"\" (cat *name* \" enters \" dest \".\") *self*)"
      , "       (set! *self* (w-dn *self* dest))"
      , "       (notify-room \"\" (cat *name* \" enters.\") *self*)"
      , "       (look))"
      , "     (if (world? n) (name n) n)))"

      , "  (define (join strs j)"
      , "    (fold (lambda (s1 s2) (cat s1 j s2)) (car strs) (cdr strs)))"

      , "  (define (take w)"
      , "    (enter"
      , "      (notify-room (cat \"You take \" *name* \".\")"
      , "                   (cat (name w) \" takes \" *name* \".\")"
      , "                   w)))"
      
      , "  (define (drop w)"
      , "    (exit)"
      , "    (notify-room (cat \"You drop \" *name* \".\")"
      , "                 (cat (name w) \" drops \" *name* \".\")"
      , "                 w))"

      , "  (define (exit)"
      , "    (notify-room \"\" (cat *name* \" leaves.\") *self*)"
      , "    (set! *self* (w-up *self*))"
      , "    (notify-room \"\" (cat *name* \" arrives.\") *self*)"
      , "    (look))"

      , "  (define (look)"
      , "    (notify (join (append (list (name (location *self*))"
      , "                                (desc (location *self*)))"
      , "                          (map (lambda (i) (cat (name i) \" is here.\"))"
      , "                               (filter (lambda (t) (not (= t *self*)))"
      , "                                       (contents (location *self*)))))"
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

      , "  (define (exits)"
      , "    ((lambda (xs)"
      , "       (if (null? xs)"
      , "           (notify \"There are no exits from your current location.\""
      , "                   *self*)"
      , "           (notify (join (cons \"The following exits are available: \""
      , "                               xs)"
      , "                         \"\n\")"
      , "                   *self*)))"
      , "     (loc-exits (location *self*))))"

      , "  (define (examine w)"
      , "    (notify *desc* w)"

      , "  (define (go dir)"
      , "    (define old-self *self*)"
      , "    (set! *self* (go-dir dir *self*))"
      , "    (notify-room \"\" (cat *name* \" goes \" dir \".\") old-self)"
      , "    (notify-room \"\" (cat *name* \" arrives.\") *self*)"
      , "    (look))"

      , ")"
      ]


invoke :: String -> [Sval] -> World -> IO (Either String (Sval,World))
invoke f sv w = let (o,cs) = bindAttrs w
  in case envLookup (pack f) 0 (bindings o) of
       Nothing -> return . Left $ "I don't know what " ++ f ++ " means."
       Just fn -> do
         res <- run (p_apply fn sv 0) (bindings o)
         return $ case res of
           (Right s,env) -> Right (s, applyAttrs env (o,cs))
           (Left err,_)  -> Left err

evalWisp :: String -> World -> IO (Either String (Sval, World))
evalWisp s w = let (o,cs) = bindAttrs w in do
  res <- run (runWisp s) (bindings o)
  return $ case res of
    (Right v, env) -> Right (v, applyAttrs env (o,cs))
    (Left err, _) -> Left err

bindAttrs :: World -> World
bindAttrs (o,cs) = let bs = M.fromList [ (sym_name, Sstring $ name o)
                                       , (sym_desc, Sstring $ description o)
                                       , (sym_self, Sworld (o,cs))
                                       ]
                       (tl,_) = bindings o M.! 0
  in (o{bindings = M.insert 0 ((bs `M.union` tl),-1) (bindings o)},cs)

applyAttrs :: Env -> World -> World
applyAttrs b w = apName . apDesc $ apWrld
  where (tl,_) = b M.! 0
        apWrld = case M.lookup sym_self tl of { Just (Sworld (f,cs)) -> (f{bindings = b},cs) ; _ -> w }
        apName w'@(f,cs) = case M.lookup sym_name tl of { Just (Sstring n) -> (f{name = n},cs); _ -> w' }
        apDesc w'@(f,cs) = case M.lookup sym_desc tl of { Just (Sstring d) -> (f{description = d},cs); _ -> w' }

sym_self :: ByteString
sym_self = pack "*self*"
sym_desc :: ByteString
sym_desc = pack "*desc*"
sym_name :: ByteString
sym_name = pack "*name*"

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

wisp_exits :: Sval
wisp_exits = Sprim $ \vs _ e -> case vs of
  [Sworld (o,_)] -> return (Right . Slist . map Sstring . M.keys $ exits o, e)
  l -> return (Left $ "bad arguments: " ++ show l, e)

wisp_go :: Sval
wisp_go = Sprim $ \vs _ e -> case vs of
  [Sstring dir, Sworld w] -> return (Sworld `fmap` go dir w, e)
  l -> return (Left $ "bad arguments: " ++ show l, e)

wisp_find :: Sval
wisp_find = Sprim $ \vs f e -> 
  let tl = fst $ e M.! 0
      self = M.lookup sym_self tl
  in case self of
    Just (Sworld w) -> case vs of
      [Sstring s] -> return (Sworld `fmap` find (matchName s) Global w, e)
      [pr@(Sfunc _ _ _)] -> return (Sworld `fmap` ioFind, e)
        -- kind of gross & weird but we actually _want_ users to be able to
        -- perform arbitrary side effects here.
        where ioFind = find (\o -> wb . fst . unsafePerformIO $ run (p_apply pr [Sworld (o,[])] f) e) Global w
              wb (Right (Sbool False)) = False
              wb (Left _) = False
              wb _ = True
      [a] -> return (Left $ "Bad argument type: " ++ show a, e)
      as -> return (Left $ "Wrong number of arguments: " ++ show (length as) ++ " for 1",e)
    Just v -> return (Left $ "Bad value for *self*: " ++ show v,e)
    Nothing -> return (Left "Missing value for *self*",e)


{- haskell-level notification primitives -}

notify :: String -> World -> IO World
notify msg w = case handle . fst $ w of Nothing -> return w
                                        Just h -> hPutStrLn h msg >> hFlush h >> return w

notifyExcept :: String -> World -> IO World
notifyExcept msg w = case zUp w of Left _ -> return w
                                   Right w' -> mapM_ (notify msg) (delete w $ zDn w') >> return w

