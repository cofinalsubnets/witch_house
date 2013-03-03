{-# LANGUAGE BangPatterns #-}
module Gretel.CommandLine (handleArgs) where

import System.Console.GetOpt
import Data.Version (showVersion)
import System.Exit
import Control.Monad (when)
import GHC.Conc (getNumCapabilities, setNumCapabilities)

import Gretel.Server.Types
import Gretel.Server.Defaults
import Gretel.Version
import System.IO

-- | Parse command line arguments.
-- TODO: Add usage. Possibly lift the Options transformers into IO.
handleArgs :: [String] -> IO Options
handleArgs args = do
  case getOpt Permute options args of
    (o,[],[]) -> foldr ($) (return defaults) o
    (_,_,_) -> exitFailure
  where options = [ Option "c" ["cores"]
                    (ReqArg setCores "N")
                      "maximum number of processor cores to use"
                  , Option "h" ["help"]
                    (NoArg (\_ -> usage >> exitSuccess))
                      "print this message and exit"
                  , Option "l" ["log-file"]
                    (ReqArg setLogFile "FILE")
                      "log file (default logs to STDERR)"
                  , Option "p" ["port"]
                    (ReqArg setPortNo "PORT")
                      "local port to listen on (default is 10101)"
                  , Option "q" ["quiet"]
                    (NoArg $ setVerbosity "0")
                      "suppress log (equivalent to verbosity 0)"
                  , Option "v" ["version"]
                    (NoArg (\_ -> vn >> exitSuccess))
                      "print version and exit"
                  , Option "V" ["verbosity"]
                    (ReqArg setVerbosity "N")
                      "verbosity of log messages"
                  , Option [] ["console"]
                    (NoArg setConsole)
                      "enable console after startup"
                  ]

        vn = putStrLn $ "Gretel " ++ showVersion version

        setCores n o = do
          let n' = read n
          c <- getNumCapabilities
          when (c /= n') $ setNumCapabilities n'
          o

        setLogFile f o = do
          opt <- o
          h <- openFile f WriteMode
          return $ opt { logHandle = h }

        setPortNo n o = do
          opt <- o
          return $ opt { portNo = read n }

        setConsole o = do
          opt <- o
          return $ opt { console = True }

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

