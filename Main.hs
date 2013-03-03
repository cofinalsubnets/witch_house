module Main where

import System.Environment (getArgs)
import Gretel.CommandLine
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
main = getArgs >>= handleArgs >>= startServer

