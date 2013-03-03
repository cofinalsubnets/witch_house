module Gretel.Server (startServer) where

import Data.Maybe (isNothing, catMaybes, fromJust)
import Data.List (delete)
import Control.Concurrent.STM
import Control.Monad
import GHC.Conc (forkIO, getNumCapabilities)
import Network
import System.IO

import qualified Data.Map as M
import qualified Data.Set as S

import Gretel.World
import Gretel.Interface
import Gretel.Server.Types
import Gretel.Server.Log
import Gretel.Server.Console

startServer :: Options -> IO ()
startServer opts = do
  chan <- atomically $ newTQueue
  wrld <- atomically $ newTMVar (world opts)
  clis <- atomically $ newTMVar (M.fromList [])
  lid <- forkIO $ listen opts chan
  let responder = respond wrld clis chan
  if not $ console opts
    then responder
    else do rid <- forkIO responder
            startConsole opts wrld clis chan lid rid

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
            _ -> do let c = Client h n
                        r = Action msg c
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
respond :: TMVar World -> TMVar ClientMap -> TQueue Req -> IO ()
respond tmw tmc q = do
  req <- atomically $ readTQueue q
  w <- atomically $ takeTMVar tmw
  case req of

    Action txt (Client h n) -> do
      let txt' = unwords [quote n,txt]
          cmd  = parseCommand rootMap txt'
          -- TODO: _correctly_ quote the name.
          quote s = "\"" ++ s ++ "\""
          (Response sm om scp ,w') = cmd w
      case scp of
        Self -> notify [h] sm
        Local -> do notify [h] sm
                    cm <- atomically $ readTMVar tmc
                    let others = delete h . handles cm $ neighbours n w'
                    notify others om
        _ -> return ()
      atomically $ putTMVar tmw w'
      respond tmw tmc q

    -- this is sort of gross atm.
    Login n h -> do
      cm <- atomically $ takeTMVar tmc
      let greeting = (hPutStrLn h $ "Hiya " ++ n ++ "!")
      if not $ M.member n w
        then let node = mkNode { name = n, location = Just $ name . root $ w }
                 w'   = addNode node w
                 cm'  = M.insert n h cm
             in do greeting
                   atomically $ putTMVar tmw w'
                   atomically $ putTMVar tmc cm'
                   respond tmw tmc q
        else if n `M.member` cm
               then do hPutStrLn h $ "Someone is already logged in as " ++ n ++". Please try again with a different handle."
                       hClose h
                       atomically $ putTMVar tmw w
                       atomically $ putTMVar tmc cm
                       respond tmw tmc q
               else if isNothing . location $ w M.! n 
                      -- TODO: Something better than this.
                      then do hPutStrLn h $ "That would crash the server. Please don't be discourteous."
                              hClose h
                              atomically $ putTMVar tmw w
                              atomically $ putTMVar tmc cm
                              respond tmw tmc q
                      else do
                        let cm' = M.insert n h cm 
                        greeting
                        atomically $ putTMVar tmw w
                        atomically $ putTMVar tmc cm'
                        respond tmw tmc q
    Logout n -> do
      cm <- atomically $ takeTMVar tmc
      let cm' = M.delete n cm
      atomically $ putTMVar tmc cm'
      atomically $ putTMVar tmw w
      respond tmw tmc q
  where

    notify hs msg = when (not $ null msg) $ mapM_ (\h -> hPutStrLn h msg >> hFlush h) hs

--    nodes = M.elems w

    locOf n w = w M.! (fromJust . location $ w M.! n)

    neighbours n w = S.toList $ contents $ locOf n w

    handles cm = catMaybes . map (flip M.lookup $ cm)

