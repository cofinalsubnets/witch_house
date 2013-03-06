module Gretel.Server (startServer) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Network
import System.IO
import System.Exit

import Gretel.World
import Gretel.Interface
import Gretel.CommandLine
import Gretel.Server.Log
import Gretel.Persistence

type Logger = Verbosity -> String -> IO ()

startServer :: Options -> IO ()
startServer opts = do
  tmw <- atomically $ newTMVar (world opts)
  pq  <- atomically $ newTQueue
  sock <- listenOn $ PortNumber (fromIntegral . portNo $ opts)

  -- TODO: add a command line option for the format string.
  let logM = logger (logHandle opts) "%H:%M:%S %z" (verbosity opts)
  logM V1 $ "Listening on port " ++ show (portNo opts) ++ "."

  _ <- forkIO $ persist (interval opts) (dbFile opts) pq
  listen sock tmw pq logM

persist :: Int -> FilePath -> TQueue World -> IO ()
persist i f q = loop 0
  where loop n = do w <- atomically $ readTQueue q
                    if i == n
                      then dumpWorld w f >> loop 0
                      else loop (n+1)
                 

-- | Listen on the given socket and spawn off threads to handle clients.
listen :: Socket -> TMVar World -> TQueue World -> Logger -> IO ()
listen sock tmw pq logM = do

  c <- getNumCapabilities
  logM V2 $ "Using up to " ++ show c ++ " cores."

  forever $ do
    (h,hn,p') <- accept sock
    hSetBuffering h LineBuffering
    logM V1 $ concat ["Connected: ", hn, ":", show p']
    -- Start the user session.
    forkFinally (session h tmw pq) (\_ -> logM V1 $ concat ["Disconnected: ", hn, ":", show p'])

-- | Log in a client and forward their requests to the responder via the
-- message queue.
session :: Handle -> TMVar World -> TQueue World -> IO ()
session h tmw pq = do
  li <- login h tmw
  case li of
    Nothing -> return ()
    Just n  -> forever $ do
      stop <- hIsClosed h
      when stop exitSuccess
      c  <- hGetLine h
      w  <- atomically $ takeTMVar tmw
      w' <- parseCommand rootMap c n w
      atomically $ writeTQueue pq w'
      atomically $ putTMVar tmw w'

-- | Handle a login request. The request will fail if someone is already
-- logged in with the given name; otherwise, the client will be attached to
-- the object with the given name (one will be created if it doesn't exist).
login :: Handle -> TMVar World -> IO (Maybe String)
login h tmw = do
  -- login
  hPutStr h "What's yr name?? "
  hFlush h
  n <- hGetLine h
  w <- atomically $ takeTMVar tmw

  case get n w >>= client of
    Nothing -> do hPutStrLn h $ "Hiya " ++ n ++ "!"
                  hFlush h
                  t <- myThreadId
                  let c = Client h t
                      w' = if n `member` w
                             then let o = get' n w in set o { client = Just $ c } w
                             else add mkObject { client = Just c , name = n } w
                  atomically $ putTMVar tmw w'
                  return $ Just n

    Just _ -> do hPutStrLn h $ "Someone is already logged in as " ++ n ++". Please try again with a different handle."
                 hClose h
                 atomically $ putTMVar tmw w
                 return Nothing

