module Gretel.Server (startServer) where

import Data.Maybe (isNothing, isJust)
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
  chan <- atomically $ newTQueue
  wrld <- atomically $ newTMVar (world opts)
  lid <- forkIO $ listen opts chan
  let responder = respond wrld chan
  if not $ console opts
    then responder
    else do rid <- forkIO responder
            startConsole opts wrld chan lid rid

-- | Accept connections on the designated port. For each connection,
-- fork off a process to forward its requests to the queue.
listen :: Options -> TQueue Req -> IO ()
listen opts chan = do
  c <- getNumCapabilities
  logMsg V2 $ "Using up to " ++ show c ++ " processor cores."
  let p = (fromIntegral . portNo $ opts) :: PortNumber
  sock <- listenOn $ PortNumber p
  logMsg V1 $ "Gretel is ready & listening on port " ++ show p ++ "!"
  forever $ do
    (h,hn,p') <- accept sock
    logMsg V1 $ concat $ ["Connected: ", hn, ":", show p']
    forkIO $ login h hn p'

  where
    -- TODO: add command line options for logfile and format string
    logMsg = logger (logHandle opts) "%H:%M:%S %z" (verbosity opts)

    login h hn p = do
      hPutStr h "What's yr name?? "
      n <- hGetLine h
      atomically $ writeTQueue chan (Login n h)
      serve h n hn p

    serve h n hn p = do
      open <- hIsOpen h
      if open
        then do
          msg <- hGetLine h
          case msg of
            "quit" -> do hPutStrLn h "Bye!"
                         hClose h
                         atomically $ writeTQueue chan (Logout n)
            "" -> serve h n hn p
            _ -> do let r = Action msg n
                    -- It's possible that the handle has been closed by
                    -- another process, e.g., the responder after a failed
                    -- login, so we need to check again. This is obviously 
                    -- suboptimal. We should implement some way of notifying
                    -- per-client servers of these events.
                    op <- hIsOpen h
                    when op $ atomically (writeTQueue chan r)
          serve h n hn p
        else logMsg V1 $ concat ["Disconnected: ", hn, ":", show p]


-- | Pull requests off of the queue, parse them into commands, update the world
-- as necessary, and send appropriate responses back to clients.
respond :: TMVar World -> TQueue Req -> IO ()
respond tmw q = do
  req <- atomically $ readTQueue q
  w <- atomically $ takeTMVar tmw
  case req of

    Action txt n -> do
      let txt' = unwords [quote n,txt]
          cmd  = parseCommand rootMap txt'
          -- TODO: _correctly_ quote the name.
          quote s = "\"" ++ s ++ "\""
          (ns,w') = cmd w
      mapM_ (notify w') ns
      atomically $ putTMVar tmw w'
      respond tmw q

    -- this is sort of gross atm.
    Login n h -> do
      let greeting = (hPutStrLn h $ "Hiya " ++ n ++ "!")
      if not $ M.member n w
        then let node = mkNode { name = n, location = Just $ name . root $ w, handle = Just h }
                 w'   = addNode node w
             in do greeting
                   atomically $ putTMVar tmw w'
                   respond tmw q
        else if isJust . handle $ w M.! n
               then do hPutStrLn h $ "Someone is already logged in as " ++ n ++". Please try again with a different handle."
                       hClose h
                       atomically $ putTMVar tmw w
                       respond tmw q
               else if isNothing . location $ w M.! n 
                      -- TODO: Something better than this.
                      then do hPutStrLn h $ "That would crash the server. Please don't be discourteous."
                              hClose h
                              atomically $ putTMVar tmw w
                              respond tmw q
                      else do
                        let w' = setHandle n (Just h) w
                        greeting
                        atomically $ putTMVar tmw w'
                        respond tmw q
    Logout n -> do
      let w' = setHandle n Nothing w
      atomically $ putTMVar tmw w'
      respond tmw q

notify :: World -> Notification -> IO ()
notify w (Notify n msg) = when (not $ null msg) $ do
  case M.lookup n w >>= handle of
    Nothing -> return ()
    Just h -> hPutStrLn h msg

setHandle :: Name -> Maybe Handle -> World -> World
setHandle n h w = let n' = w M.! n in M.insert n (n' { handle = h }) w

