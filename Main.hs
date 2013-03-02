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

import qualified Data.Map as M
import qualified Data.Set as S

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

runWorld :: World -> [WorldTransformer a] -> [a]
runWorld w [] = []
runWorld w (t:ts) = let (r,w') = t w in r:(runWorld w' ts)

data Conn = Conn { connHandle :: Handle
                 , connName   :: Name
                 } deriving (Show)

data Req = Req { text :: String
               , conn :: Conn
               } deriving (Show)

type Parser = String -> WorldTransformer String

runIO :: World -> Parser -> [Req] -> IO ()
runIO w p reqs = let cmds  = map parse reqs
                     resps = runWorld w cmds
  in mapM_ respond $ reqs `zip` resps
  where respond (req,resp) = hPutStrLn (connHandle . conn $ req) resp
        parse r = let n = connName . conn $ r
                  in p $ unwords [n, text r]

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



main = do
  opts <- getArgs >>= parseArgs
  let p = portZero + (fromIntegral . portNo $ opts)
  sock <- listenOn . PortNumber $ p
  putStrLn . startMsg $ show p
  loop parse (world opts) sock

loop pr w sock = do
   (h,hn,p) <- accept sock
   logT $ "accepted connection from " ++ hn ++ ":" ++ show p
   forkIO $ body h hn p
   loop pr w sock
  where
   body h hn p = do
       run h h pr w
       hFlush h
       hClose h
       logT $ "closed connection from " ++ hn ++ ":" ++ show p

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


run i o p w = do
  hPutStr o "> "
  hFlush o
  c <- hGetLine i
  if c == "quit"
    then hPutStrLn o "Bye!"
    else do let cmd = p c
                (r,w') = cmd w
            hPutStrLn o r
            run i o p w'

prefix p s = p ++ " " ++ s

parse = fromMaybe huh . parseCommand rootMap . prefix "player"

testWorld :: World
testWorld = let root = mkNode { name = "Root of the World"
                              , description = "\"For the leaves to touch the sky, the roots much reach deep into hell.\"\n  --Thomas Mann"
                              , contents = S.fromList ["player"]
                              }
                player= mkNode { name = "player"
                               , location = Just "Root of the World"
                               }
  in addNode player . addNode root $ M.fromList []

