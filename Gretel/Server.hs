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
import Gretel.Server.Console
import Gretel.Version

startServer :: Options -> IO ()
startServer opts = do
  tmw <- atomically $ newTMVar (world opts)
  if not $ console opts
    then server opts tmw
    else do _ <- forkIO $ server opts tmw
            putStrLn $ "Gretel " ++ showVersion version
            putStrLn "Starting console..."
            startConsole opts tmw

-- | Accept connections on the designated port. Fork off a thread to
-- handle requests for each incoming connection.
server :: Options -> TMVar World -> IO ()
server opts tmw = do
  sock <- listenOn $ PortNumber (fromIntegral . portNo $ opts)

  -- TODO: add a command line option for the format string.
  let logMsg = logger (logHandle opts) "%H:%M:%S %z" (verbosity opts)
  logMsg V1 $ "Listening on port " ++ show (portNo opts) ++ "."
  c <- getNumCapabilities
  logMsg V2 $ "Using up to " ++ show c ++ " cores."

  forever $ do
    (h,hn,p') <- accept sock
    logMsg V1 $ concat ["Connected: ", hn, ":", show p']
    -- Start the user session.
    forkFinally (session h hn p' tmw logMsg) (\_ -> logMsg V1 $ concat ["Disconnected: ", hn, ":", show p'])

-- | Attempt to log in a user. If the login succeeds, process their requests
-- until they disconnect or their connection is killed by another user.
--
-- We need to decide on a way to automatically disconnect users after (say) a
-- timeout period). Maybe store a 'last heard from' field in the client, and
-- check that under some condition (max clients reached and someone tries to
-- log in, for example).
session :: Handle -> HostName -> PortNumber -> TMVar World -> (Verbosity -> String -> IO ()) -> IO ()
session h hn p tmw logM = do
  res <- login h tmw
  case res of
    Left n -> do
      logM V2 $ hn ++ ":" ++ show p ++ " attempted to log in as " ++ n
    Right n -> do
      logM V2 $ hn ++ ":" ++ show p ++ " logged in as " ++ n
      serve h n tmw


-- | Attempt to log in a user. Checks:
-- 1. Whether a client is already associated with the supplied name,
-- in which case the attempt fails.
-- 2. Whether an object with the supplied name exists, in which case
-- that object's client is set to this user.
-- 3. Failing that, create a new object and set its client to this
-- user.
--
-- This function is probably much too long.
login :: Handle -> TMVar World -> IO (Either String String)
login h tmw = do
  hPutStr h "What's yr name?? "
  hFlush h
  n <- hGetLine h
  w <- atomically $ takeTMVar tmw
  case getClient n w of
    Nothing -> do hPutStrLn h $ "Hiya " ++ n ++ "!"
                  hFlush h
                  tid <- myThreadId -- Store thread id so it can be reliably killed.
                  let c = Player h tid
                      w'=  if hasObj n w
                             then execWorld (setClient' n c) w
                              -- TODO: set initial location in a sane way. this is totally arbitrary.
                             else let ws = addObj' n >> setLoc' n "Root of the World" >> setClient' n c
                                  in execWorld ws w
                  atomically $ putTMVar tmw w'
                  return $ Right n

    Just _ -> do atomically $ putTMVar tmw w
                 hPutStrLn h $ "Someone is already logged in as " ++ n ++". Please try again with a different handle."
                 hClose h
                 return $ Left n

-- | Handle user requests until the user disconnects, or their connection
-- is killed.
serve :: Handle -> String -> TMVar World -> IO ()
serve h n tmw = do
    do msg <- hGetLine h
       case msg of

         -- TODO: Use a command parser here like for regular commands!
         "quit" -> do w <- atomically $ takeTMVar tmw
                      let w' = execWorld (unsetClient' n) w
                      atomically $ putTMVar tmw w'
                      hPutStrLn h "Bye!"
                      kill $ getClient' n w

         "" -> serve h n tmw

         _ -> do w <- atomically $ takeTMVar tmw
                 let txt = unwords [quote n,msg]
                     cmd = parseCommand rootMap txt
                     -- TODO: _correctly_ quote the name.
                     quote s = "\"" ++ s ++ "\""
                     (ns,w') = cmd w
                 mapM_ (notify' w') ns
                 atomically $ putTMVar tmw w'
                 serve h n tmw


-- | Wrapper for notify that does nothing with an empty message.
notify' :: World -> Notification -> IO ()
notify' w (Notify n msg) = when (not $ null msg) (notifyObj n msg w)

