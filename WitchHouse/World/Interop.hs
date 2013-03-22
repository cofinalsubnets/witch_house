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
import WitchHouse.World.Global
import WitchHouse.Wisp
import WitchHouse.Wisp.Core (envLookup, p_apply, p_eval)
import Data.List (delete)
import Data.IORef

import qualified Data.Map as M

import System.IO (hPutStrLn, hFlush)

import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString)

import qualified Data.Set as S (member, insert, delete)

bootstrap :: IO ()
bootstrap = do
  mapM_ (\(k,v) -> bind toplevel k v) primitives
  eval defs toplevel
  return ()

  where
    primitives =
      [ (pack "notify", wisp_notify)
      , (pack "notify-room", wisp_notify_loc)
      , (pack "contents", wisp_w_contents)
      , (pack "neighbour", wisp_neighbour)
      , (pack "name", wisp_w_name)
      , (pack "desc", wisp_w_desc)
      , (pack "w-up", wisp_w_up)
      , (pack "w-dn", wisp_w_dn)
      , (pack "location", wisp_w_loc)
      , (pack "owner?", wisp_owner_p)
      , (pack "loc-exits", wisp_exits)
      , (pack "go-dir", wisp_go)
      , (pack "add-owner", wisp_add_owner)
      , (pack "del-owner", wisp_del_owner)
      , (pack "self", wisp_self)
      ]

    defs = unlines $
      [ "(begin"

      , "  (define (enter n)"
      , "    ((lambda (dest)"
      , "       (notify-room \"\" (cat *name* \" enters \" dest \".\") *self*)"
      , "       (set! *self* (w-dn *self* dest))"
      , "       (notify-room \"\" (cat *name* \" enters.\") *self*)"
      , "       (look))"
      , "     (if (world? n) (name n) n)))"

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

      , "  (define (exit-room p)"
      , "    (let ((new (w-up p)))"
      , "      (notify-room \"\" (cat (name p) \" leaves.\") p)"
      , "      (notify-room \"\" (cat (name p) \" arrives.\") new)"
      , "      (looks new)))"

      , "  (define (looks p)"
      , "    (notify (join (append (list (name (location p))"
      , "                                (desc (location p)))"
      , "                          (map (lambda (i) (cat (name i) \" is here.\"))"
      , "                               (filter (lambda (t) (not (= t p)))"
      , "                                       (contents (location p)))))"
      , "                  \"\n\")"
      , "            p))"


      , "  (define whoami (macro ()"
      , "    `(let ((me (self)))"
      , "       (notify (name me) me))))"

      , "  (define look (macro ()"
      , "    `(looks (self))))"

      , "  (define exit (macro ()"
      , "    `(exit-room (self))))"

      , "  (define inventory (macro ()"
      , "    `(let ((me (self)))"
      , "       (let ((cs (contents me)))"
      , "         (if (null? cs)"
      , "             (notify \"You aren't carrying anything.\" me)"
      , "             (notify (join (cons \"You are carrying:\""
      , "                                 (map name cs))"
      , "                         \"\n\")"
      , "                   me))))))"

      , "  (define exits (macro ()"
      , "    `(let ((me (self))"
      , "           (xs (loc-exits (location (self)))))"
      , "       (if (null? xs)"
      , "           (notify \"There are no exits from your current location.\" me)"
      , "           (notify (join (cons \"The following exits are available: \""
      , "                               xs)"
      , "                         \"\n\")"
      , "                   me)))))"

      
      , "  (define (examine w)"
      , "    (notify *desc* w))"

      , "  (define (go dir)"
      , "    (define old-self *self*)"
      , "    (set! *self* (go-dir dir *self*))"
      , "    (notify-room \"\" (cat *name* \" goes \" dir \".\") old-self)"
      , "    (notify-room \"\" (cat *name* \" arrives.\") *self*)"
      , "    (look))"

      , ")"
      ]


invoke :: String -> [Sval] -> World -> IO (Either String (Sval, World))
invoke f sv w = do
  let frame = objId $ focus w
  lu <- envLookup (pack f) (Just frame)
  case lu of
    Left _ -> return . Left $ "I don't know what " ++ f ++ " means."
    Right fn -> do
      bindAttrs w
      res <- p_apply p_eval [Slist (fn:sv)] frame
      case res of
        Right s -> let w' = case s of { Sworld z -> z; _ -> w }
                   in applyAttrs w' >>= return . Right . (s,)
        Left err -> return $ Left err

evalOn :: String -> World -> IO (Either String (Sval, World))
evalOn s w = do
  bindAttrs w
  let f = objId $ focus w
  res <- eval s f
  case res of
    Right v -> let w' = case v of { Sworld z -> z; _ -> w }
               in applyAttrs w' >>= return . Right . (v,)
    Left err -> return $ Left err

bindAttrs :: World -> IO ()
bindAttrs (o@(Obj{objId = f}),_) = do
  bind f sym_name $ Sstring (name o)
  bind f sym_desc $ Sstring (description o)

applyAttrs :: World -> IO World
applyAttrs w = do
  (f,_) <- getFrame (objId $ focus w)
  return . apName f $ apDesc f w
  where
    apName fm w'@(f,cs) = case M.lookup sym_name fm of { Just (Sstring n) -> (f{name = n},cs); _ -> w' }
    apDesc fm w'@(f,cs) = case M.lookup sym_desc fm of { Just (Sstring d) -> (f{description = d},cs); _ -> w' }

sym_desc :: ByteString
sym_desc = pack "*desc*"
sym_name :: ByteString
sym_name = pack "*name*"

wisp_owner_p :: Sval
wisp_owner_p = Sprim $ \vs _ -> return $ case vs of
  [Sworld w1, Sworld w2] -> Right . Sbool $ (objId $ focus w1) `S.member` (owners $ focus w2)
  _ -> Left $ "bad arguments: " ++ show vs

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

wisp_w_name :: Sval
wisp_w_name = Sprim $ \vs _ -> return $ case vs of
  [Sworld (f,_)] -> Right . Sstring . name $ f
  l -> Left $ "bad arguments: " ++ show l

wisp_w_desc :: Sval
wisp_w_desc = Sprim $ \vs _ -> return $ case vs of
  [Sworld (f,_)] -> Right . Sstring . description $ f
  l -> Left $ "bad arguments: " ++ show l

wisp_notify :: Sval
wisp_notify = Sprim $ \vs _ -> case vs of
  [Sstring s, Sworld w] -> do rw <- notify s w
                              return . Right . Sworld $ rw
  l -> return . Left $ "bad arguments: " ++ show l

wisp_notify_loc :: Sval
wisp_notify_loc = Sprim $ \vs _ -> case vs of
  [Sstring s, Sstring o, Sworld w] -> do rw <- notify s w >> notifyExcept o w
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

getSelf :: Int -> IO World
getSelf i = do
  w <- readIORef world
  case find ((i==) . objId) Global w of
    Right w' -> return w'
    Left _ -> do (_,n) <- getFrame i
                 case n of
                   Nothing -> error "can't find self!"
                   Just i' -> getSelf i'
                                                     

wisp_neighbour :: Sval
wisp_neighbour = Sprim $ \vs i -> do
  self <- getSelf i
  return $ case vs of
    [Sstring n] -> Sworld `fmap` find (matchName n) Location self
    _ -> Left $ "bad arguments: " ++ show vs

wisp_add_owner :: Sval
wisp_add_owner = Sprim $ \vs i -> case vs of
  [Sworld (f,c), Sworld t@(o,_)] -> do
    (vf,_) <- getSelf i
    if vf `owns` f then do notify ("You now own " ++ name f ++ ".") t
                           return . return $ Sworld (f{owners = S.insert (objId o) (owners f)},c)
    else return $ Left "You must own an object to grant ownership of that object."
  _ -> return . Left $ "bad arguments: " ++ show vs

wisp_self :: Sval
wisp_self = Sprim . const $ fmap (Right . Sworld) . getSelf

wisp_del_owner :: Sval
wisp_del_owner = Sprim $ \vs i -> case vs of
  [Sworld (f,c), Sworld t@(o,_)] -> do
    (vf,_) <- getSelf i
    if vf `owns` f then do notify ("You no longer own " ++ name f ++ ".") t
                           return . return $ Sworld (f{owners = S.delete (objId o) (owners f)},c)
    else return $ Left "You must own an object to revoke ownership of that object."
  _ -> return . Left $ "bad arguments: " ++ show vs


{- haskell-level notification primitives -}

notify :: String -> World -> IO World
notify msg w = case handle . fst $ w of Nothing -> return w
                                        Just h -> hPutStrLn h msg >> hFlush h >> return w

notifyExcept :: String -> World -> IO World
notifyExcept msg w = case zUp w of Left _ -> return w
                                   Right w' -> mapM_ (notify msg) (delete w $ zDn w') >> return w

