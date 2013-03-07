module Gretel.Server (startServer) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Network
import System.IO
import System.Exit
import System.Timeout
import Data.Maybe (isJust)
import Data.List (intercalate)

import Gretel.World
import Gretel.Interface
import Gretel.CommandLine (Options(..), Verbosity(..), version, showVersion)
import Gretel.Server.Log
--import Gretel.Persistence
import Control.Exception (bracket)

type Logger = Verbosity -> String -> IO ()

defaultWorld :: World
defaultWorld = set mkObject { name = "Root of the World", isRoot = True } mkWorld


startServer :: Options -> IO ()
startServer opts = do

  tmw <- atomically $ newTMVar defaultWorld
  pq  <- atomically $ newTQueue
  sock <- listenOn $ PortNumber (fromIntegral . portNo $ opts)

  -- TODO: add a command line option for the format string.
  let logM = logger (logHandle opts) "%H:%M:%S %z" (verbosity opts)
  logM V1 $ "Listening on port " ++ show (portNo opts) ++ "."

--  _ <- forkIO $ persist (interval opts) (db opts) pq
  listen sock tmw pq logM

{-
persist :: Int -> Maybe Connection -> TQueue World -> IO ()
persist i c q = loop 0
  where loop n = do w <- atomically $ readTQueue q
                    if i == n 
                      then do case c of Nothing -> return ()
                                        Just c' -> saveWorld w c'
                              loop 0
                      else loop (n+1)
                        -}
                 

-- | Listen on the given socket and spawn off threads to handle clients.
listen :: Socket -> TMVar World -> TQueue World -> Logger -> IO ()
listen sock tmw pq logM = do

  c <- getNumCapabilities
  logM V2 $ "Using up to " ++ show c ++ " cores."

  forever $ do
    (h,hn,p') <- accept sock
    hSetBuffering h LineBuffering
    hSetNewlineMode h universalNewlineMode
    logM V1 $ concat ["Connected: ", hn, ":", show p']
    -- Start the user session.
    forkFinally (session h tmw pq) (\_ -> logM V1 $ concat ["Disconnected: ", hn, ":", show p'])

-- | Log in a client and forward their requests to the responder via the
-- message queue.
session :: Handle -> TMVar World -> TQueue World -> IO ()
session h tmw _ = do
  li <- timeout 60000000 $ login h tmw -- 1 min. timeout for login
  case join li of
    Nothing -> return ()
    Just n  -> forever $ do
      stop <- hIsClosed h
      when stop exitSuccess
      c  <- timeout 600000000 $ hGetLine h -- 10 min. timeout for requests
      bracket (atomically $ takeTMVar tmw) (\w -> atomically $ putTMVar tmw w) (handleCommand n c)
    where
      handleCommand n c w = case c of Nothing -> parseCommand rootMap "quit" n w
                                      Just "" -> return w
                                      Just c' -> parseCommand rootMap c' n w


connectMsg :: String
connectMsg = "Gretel " ++ (showVersion version)

welcomeMsg :: Object -> World -> String
welcomeMsg o w = intercalate "\n" $
  [ "Welcome, " ++ name o ++ "."
  , (show . length . filter (isJust . client ) $ elems w) ++ " user(s) are currently online."
  ]

-- | Handle a login request. The request will fail if someone is already
-- logged in with the given name; otherwise, the client will be attached to
-- the object with the given name (one will be created if it doesn't exist).
login :: Handle -> TMVar World -> IO (Maybe String)
login h tmw = do
  -- login
  hPutStrLn h connectMsg
  hPutStr h "Name: "
  hFlush h

  n <- hGetLine h
  w <- atomically $ readTMVar tmw

  case get n w of
    Right o -> case password o of
      Just pw -> do hPutStr h "Password: "
                    hFlush h
                    p <- hGetLine h
                    if p == pw
                      then loginExisting o
                      else loginFailure "Incorrect password."
      Nothing -> loginExisting o

    Left _ -> loginNew n

  where

    loginFailure s = do hPutStrLn h s
                        hClose h
                        return Nothing

    loginExisting o = do w' <- atomically $ takeTMVar tmw
                         let w'' = set o { client = Just h } w'
                         atomically $ putTMVar tmw w''
                         hPutStrLn h $ welcomeMsg o w''
                         hFlush h
                         return $ Just (name o)

    loginNew s = do hPutStrLn h $ "Creating new node for " ++ s ++ "."
                    hFlush h
                    hPutStr h "Password: "
                    hFlush h
                    pw <- hGetLine h
                    w' <- atomically $ takeTMVar tmw

                    let o = mkObject { name = s
                                     , client = Just h
                                     , password = Just pw
                                     }
                        w'' = add o w'

                    atomically . putTMVar tmw $ w''
                    hPutStrLn h $ welcomeMsg o w''
                    hFlush h
                    return $ Just s

