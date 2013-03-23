module WitchHouse.Server (startServer) where

import Control.Monad
import Control.Concurrent
import Data.Time
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
import Data.ByteString.Char8 (pack)
import WitchHouse.Persistence


startServer :: Options -> IO ()
startServer opts = do
  let logger = mkLogger (logHandle opts) "%H:%M:%S %z"(verbosity opts)
  mw <- newMVar (initialState opts)
  sock <- listenOn $ PortNumber (fromIntegral . portNo $ opts)

  logger V1 $ "Listening on port " ++ show (portNo opts)

  forkIO $ garbageCollect mw

  when (persistent opts) $ do
    forkIO $ persist mw (autosave opts) (dbPath opts) logger
    return ()

  forever $ do
    (h,hn,p) <- accept sock
    logger V1 $ "Connected: " ++ show hn ++ ":" ++ show p

    -- establish sane settings for the client handle
    hSetBuffering h LineBuffering
    hSetNewlineMode h universalNewlineMode

    -- Start the user session.
    forkFinally (session h mw) (\_ -> logger V1 $ "Disconnected: " ++ show hn ++ ":" ++ show p ++ " -- ")

garbageCollect :: MVar World -> IO ()
garbageCollect mw = forever $ do
  threadDelay 300000000
  w <- takeMVar mw
  gcW w
  putMVar mw w

persist :: MVar World -> Int -> FilePath -> Logger -> IO ()
persist mw i f l = do
  threadDelay $ 1000000 * i
  l V2 $ "Saving world state to "++f++" ..."
  w <- readMVar mw
  conn <- connect f
  saveWorld w conn
  disconnect conn
  persist mw i f l

session :: Handle -> MVar World -> IO ()
session h mw = do
  client <- login h mw -- 1 min. timeout for login
  forever $ do
    hIsClosed h >>= \stop -> when stop exitSuccess
    c <- timeout 600000000 $ hGetLine h -- 10 min. timeout for requests
    modifyMVar_ mw (return . (find' (client ==) Global) >=> handleCommand c)

  where
    handleCommand c = case c of Nothing -> parseCommand "quit"
                                Just "" -> return
                                Just c' -> parseCommand c'

connectMsg :: String
connectMsg = "witch_house " ++ version

welcomeMsg :: String -> String
welcomeMsg n = "Welcome, " ++ n ++ "."

-- | Handle a login request. The request will fail if someone is already
-- logged in with the given name; otherwise, the client will be attached to
-- the object with the given name (one will be created if it doesn't exist).
login :: Handle -> MVar World -> IO Obj
login h mw = do
  c <- timeout 60000000 $ do
    hPutStrLn h connectMsg
    hPutStr h "Name: " >> hFlush h

    n <- hGetLine h
    w <- readMVar mw
    case find ((n==).name) Global w of
      Right o -> case (password.focus) o of
        Just pw -> do hPutStr h "Password: " >> hFlush h
                      p <- hGetLine h
                      if p == pw
                        then loginExisting o
                        else loginFailure "Incorrect password."
        Nothing -> loginFailure "Not a player."

      Left _ -> loginNew n

  case c of Nothing -> exitSuccess
            Just c' -> return c'

  where

    loginFailure s = do hPutStrLn h s
                        hClose h
                        exitSuccess

    loginExisting (p,_) = do bind (objId p) (pack "*handle*") (Shandle h)
                             hPutStrLn h (welcomeMsg $ name p) >> hFlush h
                             return p

    loginNew s = do hPutStrLn h ("Creating new node for " ++ s ++ ".") >> hFlush h
                    hPutStr h "Password: " >> hFlush h
                    pw <- hGetLine h

                    o <- mkPlayer s pw h
                    modifyMVar_ mw (return . zIns o . find' start Global)
                    hPutStrLn h (welcomeMsg s) >> hFlush h
                    return o

type Logger = Verbosity -> String -> IO ()

mkLogger :: Handle -> String -> Verbosity -> Logger
mkLogger h fmt vl vm msg = when (vl >= vm) $ do
  time <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale fmt time
  hPutStrLn h $ unwords [timestamp, "|", msg]

mkPlayer :: String -> String -> Handle -> IO Obj
mkPlayer n pw h = do
  o <- mkObj
  bind (objId o) (pack "*name*") (Sstring n)
  bind (objId o) (pack "*password*") (Sstring pw)
  bind (objId o) (pack "*handle*") (Shandle h)
  return o

