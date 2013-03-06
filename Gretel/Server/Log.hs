module Gretel.Server.Log (logger) where

import Data.Time
import Gretel.CommandLine (Verbosity)
import System.IO
import System.Locale (defaultTimeLocale)
import Control.Monad (when)

logger :: Handle    -- FD for log
       -> String    -- timestamp format string
       -> Verbosity -- verbosity of logger
       -> Verbosity -- minimum verbosity to log message
       -> String    -- message to log
       -> IO ()
logger h fmt vl vm msg = when (vl >= vm) $ do
  time <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale fmt time
  hPutStrLn h $ unwords [timestamp, "|", msg]

