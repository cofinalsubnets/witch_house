module Gretel.Server (startServer) where

import Control.Concurrent.STM
import Control.Monad
import GHC.Conc (forkIO, getNumCapabilities)
import Network
import System.IO

import qualified Data.Map as M

import Gretel.World
import Gretel.Interface
import Gretel.Server.Types
import Gretel.Server.Log
import Gretel.Server.Console

startServer :: Options -> IO ()
startServer opts = do
  tmw <- atomically $ newTMVar (world opts)
  if not $ console opts
    then server opts tmw
    else do _ <- forkIO $ server opts tmw
            startConsole opts tmw

-- | Accept connections on the designated port. For each connection,
-- fork off a process to forward its requests to the queue.
server :: Options -> TMVar World -> IO ()
server opts tmw = do
  sock <- listenOn $ PortNumber (fromIntegral . portNo $ opts)

  let logMsg = logger (logHandle opts) "%H:%M:%S %z" (verbosity opts)
  logMsg V1 $ "Gretel is listening on port " ++ show (portNo opts) ++ "."
  c <- getNumCapabilities
  logMsg V2 $ "Using up to " ++ show c ++ " cores."

  forever $ do
    (h,hn,p') <- accept sock
    forkIO $ session h hn p' tmw logMsg

type Logger = Verbosity -> String -> IO ()

session :: Handle -> HostName -> PortNumber -> TMVar World -> Logger -> IO ()
session h hn p tmw logM = do
  logM V1 $ concat $ ["Connected: ", hn, ":", show p]
  res <- login h tmw

  case res of
    Left n -> do
      logM V2 $ hn ++ ":" ++ show p ++ " attempted to log in as " ++ n
    Right n -> do
      logM V2 $ hn ++ ":" ++ show p ++ " logged in as " ++ n
      serve h n tmw

  logM V1 $ concat ["Disconnected: ", hn, ":", show p]


login :: Handle -> TMVar World -> IO (Either String String)
login h tmw = do
  hPutStr h "What's yr name?? "
  n <- hGetLine h
  let greeting = hPutStrLn h $ "Hiya " ++ n ++ "!"
  w <- atomically $ takeTMVar tmw
  case M.lookup n w of
    Nothing -> do greeting
                  let ws = WS (addKey n) >>
                           -- TODO: set the initial location in a sane way.
                           WS (setLoc n "Root of the World") >>
                           WS (setHandle n h)
                      w' = execWorld ws w
                  atomically $ putTMVar tmw w'
                  return $ Right n

    Just n' -> case handle n' of
      Nothing -> do greeting
                    let (_,w') = setHandle n h w
                    atomically $ putTMVar tmw w'
                    return $ Right n
      Just _ -> do atomically $ putTMVar tmw w
                   hPutStrLn h $ "Someone is already logged in as " ++ n ++". Please try again with a different handle."
                   hClose h
                   return (Left n)

serve :: Handle -> String -> TMVar World -> IO ()
serve h n tmw = do
  msg <- hGetLine h
  case msg of

    "quit" -> do hPutStrLn h "Bye!"
                 hClose h
                 w <- atomically $ takeTMVar tmw
                 let (_,w') = unsetHandle n w
                 atomically $ putTMVar tmw w'

    "" -> serve h n tmw

    _ -> do w <- atomically $ takeTMVar tmw
            let txt = unwords [quote n,msg]
                cmd  = parseCommand rootMap txt
                -- TODO: _correctly_ quote the name.
                quote s = "\"" ++ s ++ "\""
                (ns,w') = cmd w
            mapM_ (notify w') ns
            atomically $ putTMVar tmw w'
            serve h n tmw
                        

notify :: World -> Notification -> IO ()
notify w (Notify n msg) = when (not $ null msg) $ do
  case M.lookup n w >>= handle of
    Nothing -> return ()
    Just h -> hPutStrLn h msg

