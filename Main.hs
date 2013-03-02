{-# LANGUAGE BangPatterns #-}
import Gretel.World
import Gretel.Command
import Data.Maybe
import System.IO
import System.Exit
import Control.Monad
import Network
import GHC.Conc (forkIO)
import System.Console.GetOpt
import System.IO.Unsafe
import System.Environment (getArgs)
import Data.Time
import System.Locale (defaultTimeLocale)
import Network.BSD (getHostName)
import Control.Concurrent.STM

import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
  opts <- getArgs >>= parseArgs
  chan <- atomically $ newTQueue
  forkIO $ respond chan (world opts)
  listen opts chan
  where
    respond c w = do
      req <- atomically $ readTQueue c
      case req of
        Act (Conn h n) txt -> do
          let txt' = unwords [n,txt]
              cmd  = parse txt'
              (r,w') = cmd w
          hPutStrLn h r
          hFlush h
          respond c w'
        Login n h -> do
          if not $ M.member n w
            then let node = mkNode { name = n, location = Just "Root of the World" }
                     w'   = addNode node w
                 in greeting >> respond c w'
            else greeting >> respond c w
          where greeting = (hPutStrLn h $ "Hiya " ++ n ++ "!")
              

        
listen :: Gretel -> TQueue Req -> IO ()
listen opts chan = do
  let p = portZero + (fromIntegral . portNo $ opts)
  sock <- listenOn . PortNumber $ p
  putStrLn . startMsg $ show p
  forever $ do
    (h,_,_) <- accept sock
    forkIO $ login h
  where
    login h = do
      hPutStr h "What's yr name?? "
      n <- hGetLine h
      atomically $ writeTQueue chan (Login n h)
      serve h n

    serve h n = do
      msg <- hGetLine h
      if msg == "quit"
        then do hPutStrLn h "Bye!"
                hFlush h
                hClose h
        else do let c = Conn h n
                    r = Act c msg
                atomically $ writeTQueue chan r
                serve h n
  


type Parser = String -> WorldTransformer String

parse = fromMaybe huh . parseCommand rootMap


portZero :: PortNumber
portZero = 0

data Gretel = Gretel { portNo     :: !Int
                     , maxClients :: !Int
                     , world      :: !World
                     , dataDir    :: !(Maybe FilePath)
                     }

defaults = Gretel { portNo = 10101
                  , maxClients = 10
                  , world = testWorld
                  , dataDir = Nothing
                  }

data Conn = Conn { connHandle :: Handle
                 , connName   :: Name
                 } deriving (Show)

data Req = Act { conn :: Conn
               , text :: String
               } |
           Login String Handle deriving (Show)


parseArgs args = do
  case getOpt Permute options args of
    (o,[],[]) -> return $ foldr ($) defaults o
    (_,_,_) -> exitFailure
  where options = [ Option "p" ["port"]
                    (ReqArg (\n opt -> opt { portNo = read n}) "PORT")
                      "local port to listen on"
                  , Option "c" ["clients"]
                    (ReqArg (\n opt -> opt { maxClients = read n }) "CLIENTS")
                      "maximum number of parallel clients"
                  , Option "f" ["file"]
                    (ReqArg (\f opt -> opt { world = readWorld f }) "FILE")
                      "file from which to read initial world state"
                  ]

        readWorld f = unsafePerformIO $ do
          putStr $ "Reading initial state from " ++ f ++ "..."
          txt <- readFile f
          let !w = read txt :: World
          putStrLn " done!"
          return w



version = "0.0.0"

startMsg p = concat $
  [ "Gretel " 
  , version 
  , " is listening on port " 
  , p
  , "!"
  ]

logT s = do
  time <- getCurrentTime
  host <- getHostName
  let loc = defaultTimeLocale
      fmt = "%b %e %H:%M:%S"
  putStrLn $ unwords [(formatTime loc fmt time), host, ":", s]


prefix p s = p ++ " " ++ s


testWorld :: World
testWorld = let root = mkNode { name = "Root of the World"
                              , description = "\"For the leaves to touch the sky, the roots much reach deep into hell.\"\n  --Thomas Mann"
                              }
  in addNode root $ M.fromList []

