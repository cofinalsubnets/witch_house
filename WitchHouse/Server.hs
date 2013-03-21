module WitchHouse.Server (startServer) where

import Control.Monad
import Control.Concurrent
import Control.Exception (try, SomeException)
import Data.Time
import Data.IORef
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
--import WitchHouse.Persistence


startServer :: Options -> IO ()
startServer opts = do
  let logger = mkLogger (logHandle opts) "%H:%M:%S %z"(verbosity opts)
  sem <- newMVar ()
  sock <- listenOn $ PortNumber (fromIntegral . portNo $ opts)

  logger V1 $ "Listening on port " ++ show (portNo opts)

{-
  when (persistent opts) $ do
    _ <- forkIO $ persist tmw (autosave opts) (dbPath opts) logger
    return ()
    -}

  forever $ do
    (h,hn,p) <- accept sock
    logger V1 $ "Connected: " ++ show hn ++ ":" ++ show p

    -- establish sane settings for the client handle
    hSetBuffering h LineBuffering
    hSetNewlineMode h universalNewlineMode

    -- Start the user session.
    forkFinally (session h sem) (\_ -> logger V1 $ "Disconnected: " ++ show hn ++ ":" ++ show p)

{-
persist :: TMVar World -> Int -> FilePath -> Logger -> IO ()
persist tmw i f l = do
  threadDelay $ 1000000 * i
  l V2 $ "Saving world state to "++f++" ..."
  w <- atomically $ readTMVar tmw
  conn <- connect f
  saveWorld w conn
  disconnect conn
  persist tmw i f l
  -}

session :: Handle -> MVar () -> IO ()
session h sem = do
  li <- timeout 60000000 $ login h sem -- 1 min. timeout for login
  case join li of
    Nothing -> return ()
    Just n  -> forever $ do
      stop <- hIsClosed h
      when stop exitSuccess
      c <- timeout 600000000 $ hGetLine h -- 10 min. timeout for requests
      takeMVar sem
      w <- liftM (find' (n==) Global) (readIORef world) -- TODO: better error handling here
      res <- timeout 1000000 . try $ handleCommand c w :: IO (Maybe (Either SomeException World))
      case res of
        Nothing -> do
          hPutStrLn h "Operation failed due to timeout."
          writeIORef world w
        Just (Left x) -> do
          hPutStrLn h $ "An error occurred: " ++ show x
          writeIORef world w
        Just (Right w') -> writeIORef world w'
      putMVar sem ()

  where
    handleCommand c = case c of Nothing -> parseCommand "quit"
                                Just "" -> return
                                Just c' -> parseCommand c'


connectMsg :: String
connectMsg = "witch_house " ++ version

welcomeMsg :: Obj -> World -> String
welcomeMsg (Obj {name=n}) _ = "Welcome, " ++ n ++ "."

-- | Handle a login request. The request will fail if someone is already
-- logged in with the given name; otherwise, the client will be attached to
-- the object with the given name (one will be created if it doesn't exist).
login :: Handle -> MVar () -> IO (Maybe Obj)
login h sem = do
  -- login
  hPutStrLn h connectMsg
  hPutStr h "Name: " >> hFlush h

  n <- hGetLine h
  takeMVar sem
  w <- readIORef world
  case find ((n==).name) Global w of
    Right o -> case (password.focus) o of
      Just pw -> do hPutStr h "Password: " >> hFlush h
                    p <- hGetLine h
                    if p == pw
                      then loginExisting o
                      else loginFailure "Incorrect password."
      Nothing -> loginFailure "Not a player."

    Left _ -> loginNew n

  where

    loginFailure s = hPutStrLn h s >> hClose h >> return Nothing

    loginExisting (p,_) = do w <- readIORef world
                             let (p',c) = find' (p==) Global w
                             writeIORef world (p'{handle = Just h},c)
                             putMVar sem ()
                             hPutStrLn h (welcomeMsg p' (p',c)) >> hFlush h
                             return $ Just p'

    loginNew s = do hPutStrLn h ("Creating new node for " ++ s ++ ".") >> hFlush h
                    hPutStr h "Password: " >> hFlush h
                    pw <- hGetLine h

                    -- make a new obj
                    o <- mkPlayer s pw h

                    w <- readIORef world
                    let w' = zIns o $ find' start Global w
                    writeIORef world w'

                    putMVar sem ()
                    hPutStrLn h (welcomeMsg o w') >> hFlush h
                    return $ Just o

type Logger = Verbosity -> String -> IO ()

mkLogger :: Handle -> String -> Verbosity -> Logger
mkLogger h fmt vl vm msg = when (vl >= vm) $ do
  time <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale fmt time
  hPutStrLn h $ unwords [timestamp, "|", msg]

mkPlayer :: String -> String -> Handle -> IO Obj
mkPlayer n pw h = do
  o <- mkObj
  eval defs $ frameId o
  return o{name = n, password = Just pw, handle = Just h}
  where
    -- TODO: pre-parse this
    defs = unlines $
      [ "(begin"
      , "  (define (look)"
      , "    (looks *self*))"
      , "  (define (exit)"
      , "    (exit-room *self*))"
      , ")"
      ]

