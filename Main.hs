module Main where

import System.Environment (getArgs)
import Gretel.CommandLine
import Gretel.Server

-- | Startup goes like this:
--
-- 1. Parse command line arguments.
--
-- 2. Hand over control to the server, which:
--
-- 3. Forks off a process to listen for incoming connections. This process will
-- spawn additional processes as necessary to handle clients. Meanwhile:
--
-- 4. If it was enabled on the command line, an administrative console is
-- fired up. Its capabilities are currently limited (which is why it's disabled
-- by default), although it does (usefully?) allow one to send broadcast
-- messages to users.
main :: IO ()
main = getArgs >>= handleArgs >>= startServer

