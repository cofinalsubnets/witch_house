module Gretel.Interface () where
import Data.List
import System.IO
import Data.Map (Map)
import qualified Data.Map as M
import Data.Char

import Gretel.Types
import Gretel.Node

rootMap = M.fromList $
  [ ("go", go)
  , ("look", look)
  , ("take", take)
  ]

huh = "Huh?"
quit = "quit"

go :: Command
go n w [] = ("Go where?",w)
go n w [dir] = case moveNode n dir w of
  (Nothing,w') -> ("You can't go that way!",w')
  (Just n,w')  -> (show n,w')
go _ w _ = (huh,w)

look :: Command
look n w [] = case n `from` w >>= location of
  Nothing -> ("You don't seem to be anywhere!",w)
  Just l  -> (show l,w)
look n w [dir] = case dest of
  Nothing -> ("You don't see anything in that direction.",w)
  Just l  -> (show l,w)
  where dest = do node <- n `from` w
                  loc  <- location node
                  dir `from` loc
look _ w _ = (huh,w)

take n w [t] = case setLoc n t w of
  Nothing -> ("There's no " ++ t ++ " here.",w)
  Just w' -> ("You now have a " ++ t ++ ".",w')


parseCommand s = case words s of
  [] -> Nothing
  c:args -> do comm <- M.lookup c rootMap
               return $ \n w -> comm n w args

data Connection = Conn { cName    :: Name
                       , readIn   :: Handle
                       , writeOut :: Handle
                       , world    :: Node
                       }

downcase = map toLower
upcase = map toUpper

tc = handleConn $ Conn "feivel" stdin stdout testWorld

handleConn :: Connection -> IO ()
handleConn conn@(Conn n i o w) = do
  let put = hPutStrLn o
  c <- hGetLine i
  if downcase c == quit then
    put "Connection closed."
  else do
    case parseCommand c of
      Nothing -> do put huh
                    handleConn conn
      Just c' -> do let (r,w') = c' n w
                    put r
                    handleConn conn { world = w' }

