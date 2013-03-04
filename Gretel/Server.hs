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

-- | Accept connections on the designated port. For each connection,
-- fork off a process to forward its requests to the queue.
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
    forkFinally (session h hn p' tmw logMsg) (\_ -> logMsg V1 $ concat ["Disconnected: ", hn, ":", show p'])

session :: Handle -> HostName -> PortNumber -> TMVar World -> (Verbosity -> String -> IO ()) -> IO ()
session h hn p tmw logM = do
  res <- login h tmw
  case res of
    Left n -> do
      logM V2 $ hn ++ ":" ++ show p ++ " attempted to log in as " ++ n
    Right n -> do
      logM V2 $ hn ++ ":" ++ show p ++ " logged in as " ++ n
      serve h n tmw



login :: Handle -> TMVar World -> IO (Either String String)
login h tmw = do
  hPutStr h "What's yr name?? "
  hFlush h
  n <- hGetLine h
  let greeting = do hPutStrLn h $ "Hiya " ++ n ++ "!"
                    hFlush h
  w <- atomically $ takeTMVar tmw
  case getClient n w of
    Nothing -> do greeting
                  tid <- myThreadId
                  let c = Player h tid
                      w'=  if hasKey n w
                             then execWorld (setClient' n c) w
                              -- TODO: set initial location in a sane way
                             else let ws = addKey' n >> setLoc' n "Root of the World" >> setClient' n c
                                  in execWorld ws w
                  atomically $ putTMVar tmw w'
                  return $ Right n

    Just _ -> do atomically $ putTMVar tmw w
                 hPutStrLn h $ "Someone is already logged in as " ++ n ++". Please try again with a different handle."
                 hClose h
                 return $ Left n

serve :: Handle -> String -> TMVar World -> IO ()
serve h n tmw = do
    do msg <- hGetLine h
       case msg of

         "quit" -> do w <- atomically $ takeTMVar tmw
                      let w' = execWorld (unsetClient' n) w
                      atomically $ putTMVar tmw w'
                      hPutStrLn h "Bye!"
                      kill $ getClient' n w

         "" -> serve h n tmw

         _ -> do w <- atomically $ takeTMVar tmw
                 let txt = unwords [quote n,msg]
                     cmd  = parseCommand rootMap txt
                     -- TODO: _correctly_ quote the name.
                     quote s = "\"" ++ s ++ "\""
                     (ns,w') = cmd w
                 mapM_ (notify' w') ns
                 atomically $ putTMVar tmw w'
                 serve h n tmw


notify' :: World -> Notification -> IO ()
notify' w (Notify n msg) = when (not $ null msg) (notifyKey n msg w)

