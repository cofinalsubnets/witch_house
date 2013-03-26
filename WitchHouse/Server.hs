module WitchHouse.Server (startServer) where

import Control.Monad
import Control.Concurrent

import Data.Time
import Data.ByteString.Char8 (pack)

import Network

import System.IO
import System.Exit
import System.Locale (defaultTimeLocale)
import System.Timeout

import WitchHouse.World
import WitchHouse.Types
import WitchHouse.Commands
import WitchHouse.Version
import WitchHouse.Wisp
import WitchHouse.Persistence

startServer :: Options -> IO ()
startServer opts = do
  let logger = mkLogger (logHandle opts) "%H:%M:%S %z"(verbosity opts)
  mw <- newMVar (initialState opts)
  sock <- listenOn $ PortNumber (fromIntegral . portNo $ opts)


  forkIO $ garbageCollect mw

  when (persistent opts) $ do
    forkIO $ persist mw (autosave opts) (dbPath opts) logger
    return ()

  logger V1 $ "Listening on port " ++ show (portNo opts)

  forever $ do
    (h,hn,p) <- accept sock
    logger V1 $ "Connected: " ++ show hn ++ ":" ++ show p

    -- establish sane settings for the client handle
    hSetBuffering h LineBuffering
    hSetNewlineMode h universalNewlineMode

    -- Start the user session.
    forkFinally (login h mw) . const . logger V1 $ "Disconnected: " ++ show hn ++ ":" ++ show p

type Logger = Verbosity -> String -> IO ()

mkLogger :: Handle -> String -> Verbosity -> Logger
mkLogger h fmt vl vm msg = when (vl >= vm) $ do
  time <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale fmt time
  hPutStrLn h $ unwords [timestamp, "|", msg]

garbageCollect :: MVar World -> IO ()
garbageCollect mw = forever $ do
  threadDelay 300000000
  modifyMVar_ mw $ \w -> gcW w >> return w

persist :: MVar World -> Int -> FilePath -> Logger -> IO ()
persist mw i f l = connect f >>= loop
  where loop conn = forever $ do
          threadDelay $ 1000000 * i
          l V2 $ "Saving world state to " ++ f ++ " ..."
          readMVar mw >>= flip saveWorld conn

login :: Handle -> MVar World -> IO ()
login h mw = maybe exitSuccess (session h mw) =<< tryLogin
  where

    tryLogin = timeout 60000000 $ do
      n <- request h $ unlines [connectMsg, "Name: "]
      w <- readMVar mw

      case find ((n==) . name) Global w of
        Left _ -> loginNew n
        Right o -> case password . focus $ o of
          Nothing -> loginFailure "Not a player."
          Just pw -> do p <- request h "Password: "
                        if p == pw then loginExisting o
                        else loginFailure "Incorrect password."

    loginFailure s = hPutStrLn h s >> hClose h >> exitSuccess

    loginExisting (p,_) = do bind (objId p) (pack "*handle*") (Shandle h)
                             hPutStrLn h (welcomeMsg $ name p) >> hFlush h
                             return p

    loginNew s = do pw <- request h $ "Creating new node for " ++ s ++ ".\nPassword: "
                    o <- mkPlayer s pw h
                    modifyMVar_ mw (return . zIns o . find' start Global)
                    hPutStrLn h (welcomeMsg s) >> hFlush h
                    return o

request :: Handle -> String -> IO String
request h s = hPutStrLn h s >> hFlush h >> hGetLine h

connectMsg :: String
connectMsg = "witch_house " ++ version

welcomeMsg :: String -> String
welcomeMsg n = "Welcome, " ++ n ++ ".\nType `help' for help."

mkPlayer :: String -> String -> Handle -> IO Obj
mkPlayer n pw h = do
  o@(Obj{objId = f}) <- mkObj
  bind f (pack "*name*")     (Sstring n)
  bind f (pack "*password*") (Sstring pw)
  bind f (pack "*handle*")   (Shandle h)
  return o

session :: Handle -> MVar World -> Obj -> IO ()
session h mw client = forever $ do
  hIsClosed h >>= \stop -> when stop exitSuccess
  c <- timeout 600000000 $ hGetLine h -- 10 min. timeout for requests
  modifyMVar_ mw $ handleCommand c

  where
    handleCommand c = op c . find' (client ==) Global
    op Nothing      = parseCommand "quit"
    op (Just "")    = return
    op (Just c)     = parseCommand c

