module Gretel.CommandLine
( handleArgs
, Verbosity(..)
, Options(..)
, version
, showVersion
) where

import System.Console.GetOpt
import Data.Maybe (isNothing)
import System.Exit (exitSuccess, exitFailure)
import Control.Concurrent (setNumCapabilities)
import System.Posix.User
import System.Directory

import Gretel.Persistence (Connection, connect)
import System.IO

import Data.Version

gretelDir :: IO FilePath
gretelDir = do
  homeDir <- getEffectiveUserID >>= getUserEntryForID >>= return . homeDirectory
  let dir = homeDir ++ "/.gretel"
  createDirectoryIfMissing True dir
  return dir

-- | Parse command line arguments.
-- TODO: Add usage. Possibly lift the Options transformers into IO.
handleArgs :: [String] -> IO Options
handleArgs args = do
  case getOpt Permute options args of
    (o,[],[]) -> do let opts = foldr ($) (return defaults) o
                    os <- opts
                    if persistent os && isNothing (db os)
                      then do gd <- gretelDir
                              setDB (gd++"/gretel.db") opts
                      else opts
    (_,_,_) -> usage >> exitFailure
  where options = [ Option "" ["cores"]
                    (ReqArg setCores "N")
                      "maximum number of processor cores to use"
                  , Option "h" ["help"]
                    (NoArg (\_ -> usage >> exitSuccess))
                      "print this message and exit"
                  , Option "" ["log-file"]
                    (ReqArg setLogFile "FILE")
                      "log file (default logs to STDERR)"
                  , Option "p" ["port"]
                    (ReqArg setPortNo "PORT")
                      "local port to listen on (default is 10101)"
                  , Option "" ["quiet"]
                    (NoArg $ setVerbosity "0")
                      "suppress log (equivalent to verbosity 0)"
                  , Option "" ["version"]
                    (NoArg (\_ -> vn >> exitSuccess))
                      "print version and exit"
                  , Option "v" ["verbosity"]
                    (ReqArg setVerbosity "N")
                      "verbosity of log messages"
                  , Option "e" ["ephemeral"]
                    (NoArg setEphemeral)
                      "disable persistence"
                  , Option "i" ["persistence-interval"]
                    (ReqArg setInterval "N")
                      "set persistence interval (default is every 20 requests)"
                  , Option "f" ["persistence-file"]
                    (ReqArg setDB "FILE")
                      "set DB file (default is `gretel.db')"
                  ]

        vn = putStrLn $ "Gretel " ++ showVersion version

        setCores n o = do
          let n' = read n
          setNumCapabilities n'
          o

        setEphemeral o = do
          opt <- o
          return opt { persistent = False }

        setInterval n o = do
          opt <- o
          return opt { interval = read n }

        setDB f o = do
          opt <- o
          c <- connect f
          return opt { db = Just c }

        setLogFile f o = do
          opt <- o
          h <- openFile f WriteMode
          return $ opt { logHandle = h }

        setPortNo n o = do
          opt <- o
          return $ opt { portNo = read n }

        setVerbosity n o = do
          opt <- o
          v <-  case n of
            "0" -> return V0
            "1" -> return V1
            "2" -> return V2
            _ -> do 
              putStrLn $ concat [ "# Invalid verbosity level `"
                                , n
                                , "' -- defaulting to 0."
                                ]
              return V1
          return $ opt { verbosity = v }

        usage = do
          let header = "Usage: gretel [OPTIONS...]"
          putStrLn $ usageInfo header options

version :: Version
version = Version [0,0,0] ["pre-pre-alpha"]

data Verbosity = V0 | V1 | V2 deriving (Show, Eq, Enum, Ord)

data Options = Options { portNo     :: Int
                       , maxClients :: Int              -- not implemented
                       , logHandle  :: Handle
                       , verbosity  :: Verbosity
                       , persistent :: Bool
                       , interval   :: Int
                       , db         :: Maybe Connection
                       }

defaults :: Options
defaults = Options { portNo = 10101
                   , maxClients = 10
                   , logHandle = stderr
                   , verbosity = V1
                   , persistent = True
                   , interval = 20
                   , db = Nothing
                   }

