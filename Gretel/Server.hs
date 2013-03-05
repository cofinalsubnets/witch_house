module Gretel.Server (startServer) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Network
import System.IO

import Gretel.World
import Gretel.Interface
import Gretel.Server.Types
import Gretel.Server.Log

data Request = Login String Handle ThreadId |
               Action String deriving Show

type Logger = Verbosity -> String -> IO ()

startServer :: Options -> IO ()
startServer opts = do
  q <- atomically $ newTBQueue 64
  sock <- listenOn $ PortNumber (fromIntegral . portNo $ opts)

  -- TODO: add a command line option for the format string.
  let logM = logger (logHandle opts) "%H:%M:%S %z" (verbosity opts)
  logM V1 $ "Listening on port " ++ show (portNo opts) ++ "."
  _ <- forkIO $ listen sock q logM
  respond (world opts) q

-- | Handle a request and update the world.
respond :: World -> TBQueue Request -> IO ()
respond w q = do
  req <- atomically $ readTBQueue q
  w' <- case req of Login _ _ _ -> login w req
                    Action s    -> parseCommand rootMap s w
  respond w' q

-- | Handle a login request. The request will fail if someone is already
-- logged in with the given name; otherwise, the client will be attached to
-- the object with the given name (one will be created if it doesn't exist).
login :: World -> Request -> IO World
login w (Login n h t) = case getClient n w of
  Nothing -> do hPutStrLn h $ "Hiya " ++ n ++ "!"
                hFlush h
                let c = Player h t
                    w' = if hasObj n w
                           then execWorld (WS $ setClient n c) w
                           else let ws = WS (addObj n) >>
                                         -- TODO: set initial location in a sane way. this is totally arbitrary.
                                         WS (setLoc n "Root of the World") >>
                                         WS (setClient n c)
                                in execWorld ws w
                return w'

  Just _ -> do hPutStrLn h $ "Someone is already logged in as " ++ n ++". Please try again with a different handle."
               hClose h
               killThread t
               return w

login w _ = return w

-- | Listen on the given socket and spawn off threads to handle clients.
listen :: Socket -> TBQueue Request -> Logger -> IO ()
listen sock q logM = do

  c <- getNumCapabilities
  logM V2 $ "Using up to " ++ show c ++ " cores."

  forever $ do
    (h,hn,p') <- accept sock
    logM V1 $ concat ["Connected: ", hn, ":", show p']
    -- Start the user session.
    forkFinally (session h q) (\_ -> logM V1 $ concat ["Disconnected: ", hn, ":", show p'])

-- | Log in a client and forward their requests to the responder via the
-- message queue.
session :: Handle -> TBQueue Request -> IO ()
session h q = do
  -- login
  hPutStr h "What's yr name?? "
  hFlush h
  n <- hGetLine h
  -- FIXME: either correctly quote the name or disallow whitespace.
  let qn = "\"" ++ n ++ "\""
  tid <- myThreadId
  atomically $ writeTBQueue q (Login n h tid)

  -- handle requests
  -- FIXME: since we're doing login asynchronously, even if it fails, there
  -- will still potentially be a window during which `illegal' requests can
  -- make it into the queue.
  txt <- hGetContents h
  mapM_ (sendReq qn) $ filter (not . null) (lines txt)
  where sendReq n s = do let req = Action $ unwords [n,s]
                         atomically $ writeTBQueue q req

