-- Wisp is a special-purpose Lisp for scripting object behaviour in witch_house.
module WitchHouse.Wisp
( envLookup
, runWisp
, toplevelBindings
, prim_apply
, repl
) where

import WitchHouse.Types

import qualified Data.Map as M

-- SRSLY consider whether we really want to do it like this.
import System.IO
import System.IO.Unsafe

-- for the parser:
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)

runWisp :: String -> Env -> (Either String Sval, Env)
runWisp s e = case parseWisp s of Right sv -> eval' sv e
                                  Left _ -> (Left "Parse error", e)
  where eval' vs env = let (r,env') = run (prim_eval vs 0) env
                       in case r of Right l@(Slist _) -> eval' l env'
                                    _ -> (r, env')

repl :: IO ()
repl = repl' toplevelBindings
  where repl' bs = do putStr "\n>> "
                      hFlush stdout
                      l <- getLine
                      case parse sexp "" l of
                        Left err -> do putStr (show err)
                                       repl' bs
                        Right v -> do let (r,bs') = run (prim_eval v 0) bs
                                      case r of Right r' -> putStr (show r')
                                                Left err -> putStr ("error: " ++ err)
                                      repl' bs'
                                              
{- PRIMITIVES -}

prim_lambda :: Sval
prim_lambda = Sprim $ \vs f env ->
  case vs of (Slist ps):svs -> let ps' = map (\(Ssym s) -> s) ps
                               in (return $ Sfunc ps' svs f, env)
             _ -> (Left "Malformed lambda exp", env)

prim_define :: Sval
prim_define = Sprim $ \vs f env ->
  case vs of [Ssym s, xp] -> let xv = fst $ run (prim_eval xp f) env
                             in case xv of Right v -> let (frame, p) = env M.! f
                                                          frame' = M.insert s v frame
                                                          env' = M.insert f (frame',p) env
                                                    in (Right v, env')
                                           Left e -> (Left e, env)
             _ -> (Left "Bad definition syntax", env)

fold_num :: (Int -> Int -> Int) -> Sval
fold_num op = Sprim $ \vs f env ->
  let (vs',env') = evalList vs f env
      ret = do vals <- vs'
               case fst $ run (prim_apply nump vals f) env of
                 Right (Sbool True) -> return . foldl1 op $ map (\(Snum n) -> n) vals
                 Right _ -> Left $ "Bad type (expected numeric)"
                 Left err -> Left err
  in (fmap Snum ret, env')

prim_cat :: Sval
prim_cat = Sprim $ \vs f e ->
  let (vs', _) = evalList vs f e
      ret = do vals <- vs'
               case fst $ run (prim_apply stringp vals f) e of
                 Right (Sbool True) -> return . Sstring $ concatMap (\(Sstring s) -> s) vals
                 Right _ -> Left $ "Bad type (expected strings)"
                 Left err -> Left err
  in (ret, e)

prim_add :: Sval
prim_add = fold_num (+)
prim_sub :: Sval
prim_sub = fold_num (-)
prim_mul :: Sval
prim_mul = fold_num (*)
prim_div :: Sval
prim_div = fold_num quot

prim_eq :: Sval
prim_eq = Sprim $ \vs f e ->
  let (vs',e') = evalList vs f e
      ret = do vals <- vs'
               return $ Sbool . and . zipWith (==) vals $ drop 1 vals
  in (ret,e')

prim_notify :: Sval
prim_notify = Sprim $ \vs f e ->
  case evalList vs f e of
    (Left err,_) -> (Left err, e)
    (Right [Sworld (foc,_), Sstring s], _) -> case handle foc of
      Nothing -> (Right (Sstring s), e)
      Just h -> unsafePerformIO $ do hPutStrLn h s
                                     hFlush h
                                     return (Right (Sstring s), e)
    (Right l,_) -> (Left $ "bad arguments: " ++ show l, e)


prim_if :: Sval
prim_if = Sprim $ \vs f env ->
  case vs of
    [cond, y, n] -> let (v,_) = run (prim_eval cond f) env
      in case v of Left err -> (Left err, env)
                   Right v' -> case v' of (Sbool False) -> run (prim_eval n f) env
                                          _             -> run (prim_eval y f) env
    _ -> (Left "Improper syntax: if", env)


prim_name :: Sval
prim_name = Sprim $ \vs f e ->
  case evalList vs f e of
    (Left err, _) -> (Left err,e)
    (Right [Sworld w],_) -> (return $ Sstring (name . fst $ w), e)
    _ -> (Left "Bad type (expected world)", e)

prim_desc :: Sval
prim_desc = Sprim $ \vs _ e -> case vs of
  [Sworld w] -> (return $ Sstring (name . fst $ w), e)
  _ -> (Left "Bad type (expected world)", e)

-- | Function application.
-- Handles fns defined in wisp, primitive fns, and exprs that (may) evaluate
-- to fns as separate cases.
-- TODO: see if this can be shorter & add support for special forms!
prim_apply :: Sval -> [Sval] -> Int -> Expr (Either String Sval)
prim_apply (Sfunc ps body fe) vs f = Expr $ \env ->
  if length ps /= length vs then (Left $ "Wrong number of arguments: " ++ show (length vs) ++ " for " ++ show (length ps), env)
    else let (vs', env') = evalList vs f env
             ret = do vals <- vs'
                      let frame = (M.fromList (ps `zip` vals), fe)
                          (n,env'') = newFrame frame env'
                      fst $ run (foldl1 (>>) $ map (\o -> prim_eval o n) body) env''
         in (ret,env')
prim_apply (Sprim f) vs i = Expr $ \env -> f vs i env

prim_apply (Ssym s) vs i = Expr $ \env ->
  case envLookup s i env of
    Nothing -> (Left $ "Unable to resolve symbol: " ++ s, env)
    Just v  -> if v == Ssym s then (Left $ "Circular definition: " ++ s, env)
                              else run (prim_apply v vs i) env

prim_apply (Sform ps body) vs i = Expr $ \env ->
  if length ps /= length vs then (Left $ "Wrong number of arguments: " ++ show (length vs) ++ " for " ++ show (length ps), env)
    else let frame = (M.fromList $ ps `zip` vs, i)
             (n,env') = newFrame frame env
             ret = fst $ run (foldl1 (>>) $ map (\o -> prim_eval o n) body) env'
         in (ret,env')


prim_apply (Slist l) vs i = Expr $ \env -> 
  let fn = fst $ run (prim_eval (Slist l) i) env
      ret = do fn' <- fn
               return $ run (prim_apply fn' vs i) env
  in case ret of Right v -> v
                 Left err -> (Left err, env)
prim_apply v _ _ = Expr $ \env -> (Left $ "Non-applicable value: " ++ show v, env)

newFrame :: Frame -> Env -> (Int,Env)
newFrame f e = let n = succ (last $ M.keys e) in (n, M.insert n f e)
-- | Sval evaluation.
prim_eval :: Sval -> Int -> Expr (Either String Sval)
prim_eval sv f = Expr $ \env ->
  case sv of Slist (o:vs) -> run (prim_apply o vs f) env
             Ssym s -> case envLookup s f env of
                         Just v -> (return v, env)
                         Nothing -> (Left $ "Unable to resolve sumbol: " ++ s, env)
             _ -> (return sv, env)


envLookup :: String -> Int -> Env -> Maybe Sval
envLookup _ (-1) _ = Nothing
envLookup s f env = let (binds,nxt) = env M.! f
  in case M.lookup s binds of Nothing -> envLookup s nxt env
                              Just v -> Just v

evalList :: [Sval] -> Int -> Env -> (Either String [Sval], Env)
evalList vs f env = let (vs',env') = foldl acc ([],env) $ map (\o -> prim_eval o f) vs
                        acc (l,s) m = let (r,s') = run m s in (r:l,s')
                    in (sequence (reverse vs'), env')


toplevelBindings :: Env
toplevelBindings = M.fromList [(0, (frame, -1))]
  where frame = M.fromList $
          [ ("lambda", prim_lambda)
          , ("+",      prim_add   )
          , ("-",      prim_sub   )
          , ("*",      prim_mul   )
          , ("/",      prim_div   )
          , ("=",      prim_eq    )
          , ("define", prim_define)
          , ("if",     prim_if    )
          , ("notify", prim_notify)
          , ("name",   prim_name  )
          , ("cat",    prim_cat   )
          , ("boolp",  boolp      )
          , ("stringp", stringp   )
          , ("nump",   nump       )
          , ("worldp", worldp     )
          , ("funcp",  funcp      )
          , ("boolp",  boolp      )
          , ("description", prim_desc)
          ]

{- TYPE PREDICATES -}

-- FIXME: these don't eval their arguments properly

stringp :: Sval
stringp = Sprim $ \vs _ e -> (return . Sbool $ all s vs, e)
  where s v = case v of Sstring _ -> True
                        _ -> False

nump :: Sval
nump = Sprim $ \vs _ e -> (return . Sbool $ all num vs, e)
  where num v = case v of Snum _ -> True
                          _ -> False

boolp :: Sval
boolp = Sprim $ \vs _ e -> (return . Sbool $ all bln vs, e)
  where bln v = case v of Sbool _ -> True
                          _ -> False

funcp :: Sval
funcp = Sprim $ \vs _ e -> (return . Sbool $ all fn vs, e)
  where fn v = case v of Sfunc _ _ _ -> True
                         _ -> False

worldp :: Sval
worldp = Sprim $ \vs _ e -> (return . Sbool $ all wd vs, e)
  where wd v = case v of Sworld _ -> True
                         _ -> False

{- PARSER -}

parseWisp :: String -> Either ParseError Sval
parseWisp = parse sexp ""

sexp :: GenParser Char st Sval
sexp = fmap Slist $ char '(' *> expr `sepBy` whitespace <* char ')'

whitespace :: GenParser Char st String
whitespace = wsChar >> many wsChar
  where wsChar = oneOf " \n\t\r"

expr :: GenParser Char st Sval
expr = sexp <|> atom

atom :: GenParser Char st Sval
atom = str <|> symbol <|> number <|> true <|> false

str :: GenParser Char st Sval
str = Sstring `fmap` (char '"' *> many stringContents <* char '"')
  where stringContents = try (string "\\\"" >> return '"') <|> noneOf "\""

true :: GenParser Char st Sval
true = Sbool `fmap` (try (string "#t") >> return True)

false :: GenParser Char st Sval
false = Sbool `fmap` (try (string "#f") >> return False)

nonNum :: GenParser Char st Char
nonNum = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ "_+-=*/.'")

number :: GenParser Char st Sval
number = (Snum . read) `fmap` ((:) <$> digit <*> many digit)

symbol :: GenParser Char st Sval
symbol = Ssym `fmap` ((:) <$> nonNum <*> many (digit <|> nonNum))


