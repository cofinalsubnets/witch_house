{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Time
import Control.Monad
import Control.Concurrent.STM
import GHC.Conc (forkIO)
import Network
import Network.BSD (getHostName)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Locale (defaultTimeLocale)

import qualified Data.Map as M

import Gretel.Command
import Gretel.World


-- | Startup is basically a three-stage process:
-- 1. Parse command line arguments.
-- 2. Fork off a process to listen for incoming connections. This process will
-- spawn additional processes as necessary to handle these connections and
-- forward requests on to the queue, whence they will be processed by ...
-- 3. Pull requests off of the queue and send appropriate responses back to
-- clients. In general, requests will be mapped to (a) a response string, and
-- (b) an updated world state, which will be used when handling subsequent
-- requests.
main :: IO ()
main = do
  opts <- getArgs >>= parseArgs
  chan <- atomically $ newTQueue
  _ <- forkIO $ listen opts chan
  respond (world opts) chan


-- | Parse command line arguments.
-- TODO: Add usage. Possibly lift the Options transformers into IO.
parseArgs :: [String] -> IO Options
parseArgs args = do
  case getOpt Permute options args of
    (o,[],[]) -> return $ foldr ($) defaults o
    (_,_,_) -> exitFailure
  where options = [ Option "p" ["port"]
                    (ReqArg (\n opt -> opt { portNo = read n}) "PORT")
                      "local port to listen on"
                  , Option "m" ["max-clients"]
                    (ReqArg (\n opt -> opt { maxClients = read n }) "CLIENTS")
                      "maximum number of parallel clients"
                  , Option "f" ["file"]
                    (ReqArg (\f opt -> opt { world = readWorld f }) "FILE")
                      "file from which to read initial world state"
                  , Option [] ["no-log"]
                    (NoArg (\opt -> opt { logging = False }))
                      "disable console logging"
                  ]

        -- TODO: think of a better way to do this. add sanity checks.
        readWorld f = unsafePerformIO $ do
          putStr $ "Reading initial state from " ++ f ++ "..."
          txt <- readFile f
          let !w = read txt :: World
          putStrLn " done!"
          return w


-- | Accept connections on the designated port. For each connection,
-- fork off a process to forward its requests to the queue.
listen :: Options -> TQueue Req -> IO ()
listen opts chan = do
  let p = portZero + (fromIntegral . portNo $ opts) -- ugghhh
  sock <- listenOn . PortNumber $ p
  putStrLn . startMsg $ show p
  forever $ do
    (h,hn,p') <- accept sock
    when (logging opts) $ logT $ "connected: " ++ hn ++ ":" ++ show p'
    forkIO $ login h hn p'

  where

    login h hn p= do
      hPutStr h "What's yr name?? "
      n <- hGetLine h
      atomically $ writeTQueue chan (Login n h)
      serve h n hn p

    serve h n hn p = do
      msg <- hGetLine h
      if msg == "quit"
        then do hPutStrLn h "Bye!"
                hFlush h
                hClose h
                when (logging opts) $ logT $ "disconnected: " ++ hn ++ ":" ++ show p
        else do let c = Conn h n
                    r = Act c msg
                atomically $ writeTQueue chan r
                serve h n hn p


-- | Pull requests off of the queue, parse them into commands, update the world
-- as necessary, and send appropriate responses back to clients.
respond :: World -> TQueue Req -> IO ()
respond w c = do
  req <- atomically $ readTQueue c
  case req of
    Act (Conn h n) txt -> do
      let txt' = unwords [n,txt]
          cmd  = parse txt'
          (r,w') = cmd w
      when (not $ null r) $ 
        hPutStrLn h r >> hFlush h
      respond w' c
    Login n h -> do
      if not $ M.member n w
        then let node = mkNode { name = n, location = Just "Root of the World" }
                 w'   = addNode node w
             in greeting >> respond w' c
        else greeting >> respond w c

      where
        greeting = (hPutStrLn h $ "Hiya " ++ n ++ "!")
  where
    parse = parseCommand rootMap


-- Types and helper functions.

data Options = Options { portNo     :: !Int
                       , maxClients :: Int              -- not implemented
                       , world      :: !World
                       , dataDir    :: (Maybe FilePath) -- also not implemented
                       , logging    :: Bool
                       }

defaults :: Options
defaults = Options { portNo = 10101
                   , maxClients = 10
                   , world = testWorld
                   , dataDir = Nothing
                   , logging = True
                   }

data Conn = Conn { connHandle :: Handle
                 , connName   :: Name
                 } deriving (Show)

data Req = Act { conn :: Conn
               , text :: String
               } |
           Login String Handle deriving (Show)

version :: String
version = "v0.0.0a"

-- | This exists because the lack of a Read instance for PortNumber or an
-- exported intToPortNumber function means that the only way I could find
-- to make a PortNumber out of a command line argument was to do arithmetic -
-- possible because (reasonably?) PortNumber _does_ have a Num instance.
--
-- Also, as well as being a type, PortNumber turns out to be a type constructor
-- for PortID, which takes a PortNumber argument. What I'm saying is, this API is
-- a little weird.
portZero :: PortNumber
portZero = 0

startMsg :: String -> String
startMsg p = concat $
  [ "Gretel " 
  , version 
  , " is listening on port " 
  , p
  , "."
  ]


-- | Log a message to the console with a timestamp.
-- TODO: Greater configurability. Maybe a command line option for the format
-- string.
logT :: String -> IO ()
logT s = do
  time <- getCurrentTime
  host <- getHostName
  let loc = defaultTimeLocale
      fmt = "%d/%m %X"
  putStrLn $ unwords [(formatTime loc fmt time), host, ":", s]

testWorld :: World
testWorld = let root = mkNode { name = "Root of the World"
                              , description = "\"For the leaves to touch the sky, the roots much reach deep into hell.\"\n  --Thomas Mann"
                              }
  in addNode root $ M.fromList []

