{-# LANGUAGE BangPatterns #-}
module WitchHouse.Commands
( parseCommand
) where

import WitchHouse.World
import WitchHouse.Types
import WitchHouse.Wisp
import qualified Data.Map as M
import System.IO
import Control.Monad ((>=>))
import Prelude hiding (take, drop)
import qualified Data.Set as S

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many, optional)


type Command = World -> IO World

-- | Parse an input string into a command using the given
-- command map.
parseCommand :: String -> Command
parseCommand s = case parse command "" s of
  Left e -> notify (show e)-- notify $ "I don't know what `" ++ s ++ "' means."
  Right c -> c

{- NOTIFICATION HELPERS -}

notifyResult :: (World -> Either String World) -> Command -> Command
notifyResult wt wio w = case wt w of Left err -> notify err w
                                     Right w' -> wio $ find' (==focus w) Global w'

{- COMMANDS -}

help :: Command
help = notify helpMsg
  where
    helpMsg = unlines $
      [ "In the following examples, angle brackets (`<' and `>') denote required arguments,"
      , "and square brackets (`[' and `]') denote optional arguments."
      , "A superset of these commands is available:"
      , ""
      , "basic commands:"
      , "  look"
      , "  go        <direction>"
      , "  take      <thing>"
      , "  drop      <thing>"
      , "  enter     <thing>"
      , "  exit"
      , "  say       [message]"
      , "  /me       [whatever it is that you do]"
      , "  help"
      , "  whoami"
      , ""
      , "building commands:"
      , "  make     <thing>"
      , "  link     <origin> <destination> <direction>"
      , "  unlink   <origin> <direction>"
      , "  recycle  <thing>"
      , ""
      , "lisp interaction (s-expressions will be automatically evaluated):"
      , "  bindings"
      , "  reset"
      , ""
      , "Entering a nullary command not listed here will attempt to call the lisp function"
      , "of the same name - if present in your environment - with no arguments. Note that"
      , "several basic commands are implemented in lisp, so *redefine them at your own risk*."
      , "You can use the `reset' command to restore your environment if it becomes corrupted."
      ]

send :: String -> Command
send actn w = do
  res <- invoke actn [] w
  case res of Left err -> notify err w
              Right (_,w') -> return w'

bindings :: Command
bindings w = do
  (m,_) <- getFrame (frameId $ focus w)
  notify (show m) w


quit :: Command
quit (f,c) = case handle f of
  Nothing -> return (f,c)
  Just h -> do hPutStrLn h "Bye!"
               hClose h
               return (f{handle = Nothing},c)

goes :: String -> Command
goes dir w = do
  res <- invoke "go" [Sstring dir] w
  case res of
    Left err -> notify err w
    Right (_,w') -> return w'

enters :: String -> Command
enters n w = case enter (matchName n) w of
  Left err -> notify err w
  Right w' -> do (++ " enters "++(name . focus . zUp' $ w')++".") . name . focus >>= notifyExcept $ w
                 send "look" w' >>= ((++" enters.") . name . focus >>= notifyExcept)

makes :: String -> Command
makes n = make n >=> notify ("You make "++n++".")

recycle :: String -> Command
recycle n w = case find (matchName n) Self w of
  Left err -> notify err w
  Right w' -> case handle . focus $ w' of
                Just _ -> notify "You can't recycle an active player!" w >> notify ((name . focus $ w) ++ " tried to recycle you!") w'
                Nothing -> case contents . focus $ w' of
                             [] -> do notify ((name $ focus w') ++ " has been recycled.") w
                                      dropFrame . frameId $ focus w'
                                      return $ zDel w'
                             _ -> notify "You can't recycle a non-empty object." w

reset :: Command
reset (o@(Obj{frameId = f}),cs) = do
  dropFrame f
  f' <- pushFrame (M.fromList [], Just toplevel)
  return (o{frameId = f'}, cs) >>= notify "Bindings reset."

links :: String -> String -> Command
links dir dest = notifyResult (\w -> zUp w >>= link dir (matchName dest) >>= find (focus w ==) Self) $
                   notify ("Linked: "++dir++" => "++dest)

unlinks :: String -> Command
unlinks dir = notifyResult (\w -> zUp w >>= unlink dir >>= find (focus w==) Self) (notify $ "Unlinked: "++dir)

say :: String -> Command
say msg = notify ("You say \""++msg++"\"") >=> ((++" says \""++msg++"\"") . name . focus >>= notifyExcept)

me :: String -> Command
me msg = notify msg >=> notifyExcept msg

evals :: String -> Command
evals s w = do
  res <- s `evalOn` w
  case res of Left err -> notify err w
              Right (v,w') -> notify (show v) w >> return w'

evalIn :: String -> String -> Command
evalIn l t w = case find (matchName t) Location w of
  Left err -> notify err w
  Right t' -> if (objId $ focus w) `S.member` (owners $ focus t') then evals l t'
              else notify "You aren't allowed to do that." w

takes :: String -> Command
takes n w = case take (matchName n) w of
  Left err -> notify err w
  Right w' -> do notify ("You take " ++ (name $ focus w') ++ ".") w
                 notifyExcept ((name $ focus w) ++ " takes " ++ (name $ focus w')) (zUp' w')
                 notify ((name $ focus w) ++ " takes you!") w'

drops :: String -> Command
drops n w = case drop (matchName n) w of
  Left err -> notify err w
  Right w' -> do notify ("You drop " ++ (name $ focus w') ++ ".") w               
                 notifyExcept ((name $ focus w) ++ " drops " ++ (name $ focus w')) w
                 notify ((name $ focus w) ++ " drops you!") w'
                 send "look" w'


command :: GenParser Char st Command
command = optional whitespace *> (wispExpr <|> targetedExpr <|> cmd)

  where
    whitespace = many1 $ oneOf " \n\r\t"

    wispExpr = evals `fmap` ((:) <$> char '(' <*> many anyChar)

    targetedExpr = do
      char '@'
      target <- str
      expr <- many anyChar
      return $ evalIn expr target

    eof' = optional whitespace <* eof

    unary s   = try $ string s *> whitespace *> str <* eof'
    nullary s = try $ string s <* eof'
    binary s  = try $ do
      string s
      whitespace
      s1 <- str
      s2 <- str
      eof'
      return [s1,s2]


    stringQuotedBy c = char c *> many cs <* char c
      where cs = try (string ['\\',c] >> return c) <|> noneOf [c]

    str =  stringQuotedBy '\''
       <|> stringQuotedBy '\"'
       <|> stringQuotedBy '`'
       <|> (many (noneOf " \n\r\t") <* optional whitespace)


    cmd =  cEnter
       <|> cGo
       <|> cMake
       <|> cTake
       <|> cDrop
       <|> cQuit
       <|> cLink
       <|> cUnlink
       <|> cSay
       <|> cEmote
       <|> cReset
       <|> cRecycle
       <|> cBindings
       <|> cHelp
       <|> cSend

    cEnter    = enters `fmap` unary "enter"
    cGo       = goes `fmap` unary "go"
    cMake     = makes `fmap` unary "make"
    cTake     = takes `fmap` unary "take"
    cDrop     = drops `fmap` unary "drop"
    cQuit     = nullary "quit" >> return quit
    cLink     = binary "link" >>= \[dir,dest] -> return $ links dir dest
    cUnlink   = unlinks `fmap` unary "unlink"
    cSay      = say `fmap` try (string "say" *> whitespace *> many1 anyChar)
    cEmote    = me `fmap` try (string "\\me" *> whitespace *> many1 anyChar)
    cReset    = nullary "reset" >> return reset
    cRecycle  = recycle `fmap` unary "recycle"
    cBindings = nullary "bindings" >> return bindings
    cHelp     = nullary "help" >> return help
    cSend     = many anyChar >>= return . send

