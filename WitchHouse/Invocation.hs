module WitchHouse.Invocation (handleArgs) where

import System.Console.GetOpt
import System.Exit (exitSuccess, exitFailure)
import Control.Concurrent (setNumCapabilities)
import Control.Exception (SomeException, evaluate, catch)
import System.Posix.User
import System.Directory
import Data.ByteString.Char8 (pack)
import System.IO

import WitchHouse.Types
import WitchHouse.Persistence (connect, disconnect, loadWorld)
import WitchHouse.Wisp (load, evalWisp, toplevel, repl, bind)
import WitchHouse.World
import WitchHouse.Version
import WitchHouse.Wisp.Test

handleArgs :: [String] -> IO Options
handleArgs args = case getOpt Permute options args of
  (o,[],[]) -> foldl (>>=) defaults o
  _         -> displayUsage >> exitFailure

options :: [OptDescr (Options -> IO Options)]
options = [ Option "" ["cores"]
            (ReqArg setCores "N")
              "maximum number of processor cores to use"
          , Option "r" ["run"]
            (ReqArg runFile "FILE")
              "run wisp from a file and exit"
          , Option "h" ["help"]
            (NoArg (\_ -> displayUsage >> exitSuccess))
              "print this message and exit"
          , Option "p" ["port"]
            (ReqArg setPortNo "PORT")
              "local port to listen on (default is 10101)"
          , Option "e" ["ephemeral"]
            (NoArg setEphemeral)
              "disable persistence"
          , Option "i" ["autosave-interval"]
            (ReqArg setInterval "SECONDS")
              "set autosave interval"
          , Option "d" ["db-file"]
            (ReqArg setDB "FILE")
              "set DB file"
          , Option "l" ["load"]
            (ReqArg loadDB "FILE")
              "load initial state from file"
          , Option "" ["version"]
            (NoArg (\_ -> putStrLn ("witch_house " ++ version) >> exitSuccess))
              "print version and exit"
          , Option "" ["log-file"]
            (ReqArg logTo "FILE")
              "log to FILE instead of STDOUT"
          , Option "v" ["verbosity"]
            (ReqArg setVerbosity "N")
              "set log verbosity (0-2)"
          , Option "" ["quiet"]
            (NoArg $ setVerbosity "0")
              "disable logging"
          , Option "" ["wisp"]
            (NoArg (\_ -> repl >> exitSuccess))
              "wisp REPL"
          ]

runFile f opts = do
  load toplevel testLib
  t <- readFile f
  r <- evalWisp toplevel (unlines ["(do", t, ")"])
  case r of Right _ -> exitSuccess
            Left err -> putStrLn err >> exitFailure

loadDB :: FilePath -> Options -> IO Options
loadDB f opts = do
  c <- connect f
  w <- loadWorld c
  disconnect c
  return opts{initialState = w}

logTo :: FilePath -> Options -> IO Options
logTo f opts = do
  h <- openFile f WriteMode
  return opts{logHandle = h}

setDB :: FilePath -> Options -> IO Options
setDB f opts = return opts{dbPath = f}

setVerbosity :: String -> Options -> IO Options
setVerbosity v o = do
  vrb <- case v of "0" -> return V0
                   "1" -> return V1
                   "2" -> return V2
                   _ -> do putStrLn $ "Unable to parse argument: "++v
                           exitFailure
  return o{verbosity=vrb}

setCores :: String -> Options -> IO Options
setCores n o = do
  n' <- readArg n
  setNumCapabilities n'
  return o
 
setInterval :: String -> Options -> IO Options
setInterval n opts = do
  s <- readArg n 
  return opts{autosave = s}

setEphemeral :: Options -> IO Options
setEphemeral opts = return opts{persistent = False}

setPortNo :: String -> Options -> IO Options
setPortNo n opts = do
  no <- readArg n
  return $ opts{portNo = no}

displayUsage :: IO ()
displayUsage = putStrLn $ usageInfo header options
  where header = "Usage: witch_house [OPTIONS...]"

readArg :: Read a => String -> IO a
readArg s = evaluate (read s) `catch` readFail
  where readFail e = do _ <- return (e::SomeException) -- to convince the typechecker
                        hPutStrLn stderr $ "Unable to parse argument: " ++ s
                        displayUsage
                        exitFailure

configDir :: IO FilePath
configDir = do
  homeDir <- getEffectiveUserID >>= getUserEntryForID >>= return . homeDirectory
  let dir = homeDir ++ "/.witch_house"
  createDirectoryIfMissing False dir
  return dir

defaults :: IO Options
defaults = do
  c <- configDir
  w <- blankWorld
  return Options { portNo       = 10101
                 , dbPath       = c ++ "/witch_house.db"
                 , persistent   = True
                 , autosave     = 300
                 , initialState = w
                 , verbosity    = V1
                 , logHandle    = stdout
                 }

blankWorld :: IO World
blankWorld = do
  root <- mkObj
  bind (objId root) (pack "*name*") (Str "Root")
  return (root{start = True}, [])

