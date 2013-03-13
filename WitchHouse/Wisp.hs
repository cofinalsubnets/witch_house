-- Wisp is a special-purpose Lisp for scripting object behaviour in witch_house.
module WitchHouse.Wisp
( envLookup
, runWisp
, toplevel
, invoke
, repl
) where

import WitchHouse.Types

import qualified Data.Map as M
import Data.List

-- SRSLY consider whether we really want to do it like this.
import System.IO
import System.IO.Unsafe

-- for the parser:
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)

invoke :: String -> [Sval] -> Env -> Either String Sval
invoke f sv e = case envLookup f 0 e of
  Nothing -> Left $ "Unable to resolve symbol: " ++ f
  Just fn -> fst $ run (prim_apply fn sv 0) e

runWisp :: String -> Expr (Either String Sval)
runWisp s = Expr $ \e -> case parseWisp s of Right sv -> let (v,e') = run (prim_eval sv 0) e in (v, gc e')
                                             Left err -> (Left $ show err, e)

repl :: IO ()
repl = repl' toplevel
  where repl' bs = do putStr "\n>> "
                      hFlush stdout
                      l <- getLine
                      case l of
                        ".env" -> putStr (show bs) >> repl' bs
                        ".quit" -> return ()
                        "" -> repl' bs
                        _ -> case run (runWisp l) bs of
                               (Left err, bs') -> do putStr err
                                                     repl' bs'
                               (Right v, bs') -> do putStr (show v)
                                                    repl' bs'

{- PRIMITIVES -}

prim_quote :: Sval
prim_quote = Sprim $ \vs _ e ->
  case vs of [v] -> (Right v,e)
             _ -> (Left $ "Wrong number of arguments: " ++ show (length vs) ++ " for 1", e)

prim_lambda :: Sval
prim_lambda = Sprim $ \vs f env ->
  case vs of (Slist ps):svs -> let ps' = map (\(Ssym s) -> s) ps
                               in (return $ Sfunc ps' svs f, env)
             _ -> (Left "Malformed lambda exp", env)

prim_define :: Sval
prim_define = Sprim $ \vs f env ->
  case vs of [Ssym s, xp] -> let (xv,env') = run (prim_eval xp f) env
                             in case xv of Right v -> let (frame, p) = env M.! f
                                                          frame' = M.insert s v frame
                                                          env'' = M.insert f (frame',p) env'
                                                    in (Right v, env'')
                                           Left e -> (Left e, env)
             _ -> (Left "Bad definition syntax", env)

prim_set :: Sval
prim_set = Sprim $ \vs f env ->
  case vs of [Ssym s, xp] -> let (xv,env') = run (prim_eval xp f) env
                             in case xv of Right v -> case findBind s f env' of
                                                        Nothing -> (Left $ "Unable to resolve symbol: " ++ s, env)
                                                        Just n -> let (fr,n') = env M.! n
                                                                  in (Right v, M.insert n (M.insert s v fr, n') env')
                                           Left e -> (Left e, env)
             _ -> (Left "Bad definition syntax", env)

  where findBind _ (-1) _ = Nothing
        findBind nm n e = let (bs,n') = e M.! n in if nm `M.member` bs then Just n else findBind nm n' e

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

prim_null :: Sval
prim_null = Sprim $ \vs f e ->
  case vs of
    [l] -> case run (prim_eval l f) e of
             (Left err, e') -> (Left err, e')
             (Right v, e') -> case v of
                                (Slist []) -> (Right (Sbool True),e')
                                (Slist _) -> (Right (Sbool False),e')
                                _ -> (Left $ "Bad argument: " ++ show v, e')
    _ -> (Left $ "Wrong number of arguments: " ++ show (length vs) ++ " for 1", e)
            

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
  let (posArgs, splat) = break (== ".") ps
      appl pos var sup
        | (not $ null var) && (not $ length var == 2) =
            (Left $ "Bad variadic parameter syntax: " ++ show ps, env)
        | length pos > length sup || null var && length pos < length sup =
            (Left $ "Wrong number of arguments: " ++ show (length sup) ++ " for " ++ show (length pos), env)
        | otherwise = let (vs', env') = evalList vs f env
                          ret = do vals <- vs'
                                   let posV = pos `zip` vals
                                       varV = if null var then [] else [(last var, Slist $ drop (length pos) vals)]
                                       frame = (M.fromList (varV ++ posV), fe)
                                       (n,env'') = newFrame frame env'
                                   return $ run (foldl1 (>>) $ map (\o -> prim_eval o n) body) env''
                      in case ret of Left err -> (Left err, env)
                                     Right res -> res
  in appl posArgs splat vs

prim_apply (Sprim f) vs i = Expr $ \env -> f vs i env

prim_apply (Ssym s) vs i = Expr $ \env ->
  case envLookup s i env of
    Nothing -> (Left $ "Unable to resolve symbol: " ++ s, env)
    Just v  -> if v == Ssym s then (Left $ "Circular definition: " ++ s, env)
                              else run (prim_apply v vs i) env

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
                         Nothing -> (Left $ "Unable to resolve symbol: " ++ s, env)
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

-- | VERY primitive reference-counting garbage collection.
gc :: Env -> Env
gc env = let ps = gc' env 0 in foldl (flip M.delete) env (M.keys env \\ ps)
  where gc' e n = let (frame,_) = e M.! n 
                      fs = filter (not . (n==)) $ map (\(Sfunc _ _ i) -> i) . filter func $ M.elems frame
                  in n:(concatMap (gc' e) fs)
        func sv = case sv of Sfunc _ _ _ -> True
                             _ -> False

toplevel :: Env
toplevel = let primitives = M.fromList [(0, (ps, -1))]
               defs = runWisp "(define list (lambda (. l) l))"
           in snd $ run defs primitives
  where ps = M.fromList $
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
          , ("quote",  prim_quote )
          , ("boolp",  boolp      )
          , ("stringp", stringp   )
          , ("nump",   nump       )
          , ("worldp", worldp     )
          , ("funcp",  funcp      )
          , ("boolp",  boolp      )
          , ("null",   prim_null  )
          , ("description", prim_desc)
          , ("set!", prim_set     )
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
parseWisp = parse wisp ""
  where
    wisp = many whitespace *> expr

    whitespace = wsChar >> many wsChar
      where wsChar = oneOf " \n\t\r"

    expr = nakedExpr <|> quotedExpr

    nakedExpr = sexp <|> atom
    quotedExpr = (\v -> Slist [prim_quote, v]) `fmap` (quote *> nakedExpr)
      where quote = char '\''

    sexp = fmap Slist $ char '(' *> expr `sepBy` whitespace <* char ')'

    atom = str <|> symbol <|> number <|> true <|> false

    str = Sstring `fmap` (char '"' *> many stringContents <* char '"')
      where stringContents = try (string "\\\"" >> return '"') <|> noneOf "\""

    symbol = Ssym `fmap` ((:) <$> symC <*> many (digit <|> symC))
      where symC = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ "_+-=*/.!")

    number = (Snum . read) `fmap` ((:) <$> digit <*> many digit)

    true = Sbool `fmap` (try (string "#t") >> return True)
    false = Sbool `fmap` (try (string "#f") >> return False)





