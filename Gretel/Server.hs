module Gretel.Server
( startServer
, defaults
, Options(..)   --
, Verbosity(..) -- To permit command line parsing.
, World         --
) where

import Data.Maybe (isNothing, catMaybes, fromJust)
import Data.Time
import Data.List (delete)
import Control.Concurrent.STM
import Control.Monad
import GHC.Conc (forkIO)
import Network
import System.IO
import System.Locale (defaultTimeLocale)

import qualified Data.Map as M
import qualified Data.Set as S

import Gretel.Server.Types
import Gretel.World
import Gretel.Command

startServer :: Options -> IO ()
startServer opts = do
  chan <- atomically $ newTQueue
  _ <- forkIO $ listen opts chan
  respond (world opts) (M.fromList []) chan

-- | Accept connections on the designated port. For each connection,
-- fork off a process to forward its requests to the queue.
listen :: Options -> TQueue Req -> IO ()
listen opts chan = do
  let p = portZero + (fromIntegral . portNo $ opts) -- ugghhh
  sock <- listenOn . PortNumber $ p
  logT opts V1 $ "Listening on port " ++ show p ++ "."
  forever $ do
    (h,hn,p') <- accept sock
    logT opts V1 $ "Connected: " ++ hn ++ ":" ++ show p'
    forkIO $ login h hn p'

  where

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
          if msg == "quit"
            then do hPutStrLn h "Bye!"
                    hFlush h
                    hClose h
            else do let c = Client h n
                        r = Action msg c
                    -- It's possible that the handle has been closed by
                    -- another process, e.g., the responder after a failed
                    -- login, so we need to check again. This is obviously 
                    -- suboptimal. We should implement some way of notifying
                    -- per-client servers of these events.
                    op <- hIsOpen h
                    when op $ atomically (writeTQueue chan r)
          serve h n hn p
        else logT opts V1 $ "disconnected: " ++ hn ++ ":" ++ show p


-- | Pull requests off of the queue, parse them into commands, update the world
-- as necessary, and send appropriate responses back to clients.
respond :: World -> ClientMap -> TQueue Req -> IO ()
respond w cm q = do
  req <- atomically $ readTQueue q
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
                    let others = delete h $ handles . neighbours $ n
                    notify others om
        _ -> return ()
      respond w' cm q

    -- this is sort of gross atm.
    Login n h -> do
      let greeting = (hPutStrLn h $ "Hiya " ++ n ++ "!")
      if not $ M.member n w
        then let node = mkNode { name = n, location = Just $ name . root $ w }
                 w'   = addNode node w
                 cm'  = M.insert n h cm
             in greeting >> respond w' cm' q
        else if n `M.member` cm
               then do hPutStrLn h $ "Someone is already logged in as " ++ n ++". Please try again with a different handle."
                       hFlush h
                       hClose h
                       respond w cm q
               else if isNothing . location $ w M.! n 
                      -- TODO: Something better than this.
                      then do hPutStrLn h $ "That would crash the server. Please don't be discourteous."
                              hFlush h
                              hClose h
                              respond w cm q
                      else let cm' = M.insert n h cm in greeting >> respond w cm' q
  where

    notify hs msg = when (not $ null msg) $ mapM_ (\h -> hPutStrLn h msg >> hFlush h) hs

--    nodes = M.elems w

    locOf n = w M.! (fromJust . location $ w M.! n)

    neighbours = S.toList . contents . locOf

    handles = catMaybes . map (flip M.lookup $ cm)


-- | Log a message to the console with a timestamp.
-- TODO: More configurability. Maybe a command line option for the time format
-- string.
logT :: Options -> Verbosity -> String -> IO ()
logT opts v s = when (verbosity opts >= v) $ do
  time <- getCurrentTime
  let loc = defaultTimeLocale
      fmt = "%d/%m %X"
  putStrLn $ unwords [(formatTime loc fmt time), "|", s]

defaults :: Options
defaults = Options { portNo = 10101
                   , maxClients = 10
                   , world = defaultWorld
                   , dataDir = Nothing
                   , verbosity = V1
                   }

defaultWorld :: World
defaultWorld = let r = mkNode { name = "Root of the World"
                              , description = "\"For the leaves to touch the sky, the roots much reach deep into hell.\"\n  --Thomas Mann"
                              }
  in addNode r $ M.fromList []

-- | This exists because the lack of a Read instance for PortNumber or an
-- exported intToPortNumber function means that the only way I could find
-- to make a PortNumber out of a command line argument was to do arithmetic -
-- possible because (reasonably?) PortNumber _does_ have a Num instance.
--
-- Also, as well as being a type, PortNumber turns out to be a type constructor
-- for PortID, which takes a PortNumber argument. What I'm saying is, this API
-- is a little weird.
portZero :: PortNumber
portZero = 0

