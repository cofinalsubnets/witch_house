{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Version
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit

import Gretel.Server


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
main = getArgs >>= parseArgs >>= startServer


-- | Parse command line arguments.
-- TODO: Add usage. Possibly lift the Options transformers into IO.
parseArgs :: [String] -> IO Options
parseArgs args = do
  case getOpt Permute options args of
    (o,[],[]) -> foldr ($) (return defaults) o
    (_,_,_) -> exitFailure
  where options = [ Option "p" ["port"]
                    (ReqArg (\n opt -> opt >>= \o -> return $ o { portNo = read n}) "PORT")
                      "local port to listen on"
                  {- -- not implemented yet
                  , Option "m" ["max-clients"]
                    (ReqArg (\n opt -> opt >>= \o -> return $ o { maxClients = read n }) "CLIENTS")
                      "maximum number of parallel clients"
                  -}
                  , Option "f" ["file"]
                    (ReqArg readWorld "FILE")
                      "file containing initial world state"
                  , Option "q" ["quiet"]
                    (NoArg $ setVerbosity "0")
                      "suppress console logging"
                  , Option "v" ["version"]
                    (NoArg (\_ -> vn >> exitSuccess))
                      "print version and exit"
                  , Option "V" ["verbosity"]
                    (ReqArg setVerbosity "N")
                      "verbosity of log messages"
                  , Option "h" ["help"]
                    (NoArg (\_ -> usage >> exitSuccess))
                      "print this message and exit"
                  ]

        vn = putStrLn $ "Gretel " ++ showVersion version

        setVerbosity n o = do
          opt <- o
          v <-  case n of
            "0" -> return V0
            "1" -> return V1
            "2" -> return V2
            _ -> do 
              putStrLn $ concat [ "# Invalid verbosity level `"
                                , n
                                , "' -- defaulting to level 1"
                                ]
              return V1
          return $ opt { verbosity = v }

        usage = do
          let header = "Usage: gretel [OPTIONS...]"
          putStrLn $ usageInfo header options

        -- TODO: Add sanity checks.
        readWorld f o = do
          opt <- o
          txt <- readFile f
          putStr $ "# Reading initial state from " ++ f ++ "..."
          let !w = read txt :: World
          putStrLn " done!"
          return $ opt { world = w }


-- Types and helper functions.

startMsg :: String -> String
startMsg p = concat $
  [ "Gretel " 
  , showVersion version 
  , " is listening on port " 
  , p
  , "."
  ]

version :: Version
version = Version [0,0,0] ["pre-pre-alpha"]

