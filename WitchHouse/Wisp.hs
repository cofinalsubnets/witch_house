module WitchHouse.Wisp
( runWisp
, toplevelBindings
) where

import Data.Maybe
import qualified Data.Map as M
import Data.Monoid

import WitchHouse.Wisp.Types
import WitchHouse.Wisp.Parser

pushFrame :: Expr ()
pushFrame = Expr $ \e -> ((),(M.fromList []):e)

popFrame :: Expr ()
popFrame = Expr $ \(f:fs) -> ((),fs)

getEnv :: Expr Env
getEnv = Expr $ \e -> (e,e)

setEnv :: Env -> Expr ()
setEnv e = Expr $ \_ -> ((),e)

runWisp :: String -> Env -> (Either String Sval, Env)
runWisp s e = case parseWisp s of Right sv -> eval' sv e
                                  Left _ -> (Left "Parse error", e)
  where eval' vs env = let (r,env') = run (eval vs) env
                       in case r of Right l@(Slist _) -> eval' l env'
                                    _ -> (r, env')

runToplevel :: String -> (Either String Sval, Env)
runToplevel s = runWisp s toplevelBindings

eval :: Sval -> Expr (Either String Sval)
eval sv = Expr $ \env ->
  case sv of Slist (o:vs) -> run (apply o vs) env
             Ssym s -> case envLookup s env of
                         Just v -> (return v, env)
                         Nothing -> (Left $ "Unable to resolve sumbol: " ++ s, env)
             _ -> (return sv, env)

envLookup :: String -> Env -> Maybe Sval
envLookup _ [] = Nothing
envLookup s (f:fs) = case M.lookup s f of Nothing -> envLookup s fs
                                          Just v -> Just v

evalList :: [Sval] -> Env -> (Either String [Sval], Env)
evalList vs env = let (vs',env') = foldl (\(l,s) m -> let (r,s') = run m s in (r:l,s')) ([],env) $ map eval vs
                  in (sequence vs', env')


lambda :: Sval
lambda = Sprim $ \vs env ->
  case vs of (Slist ps):svs -> let ps' = map (\(Ssym s) -> s) ps
                               in (return $ Sfunc ps' svs env, env)
             _ -> (Left "Malformed lambda exp", env)

define :: Sval
define = Sprim $ \vs env@(f:fs) ->
  case vs of [Ssym s, xp] -> let xv = fst $ run (eval xp) env
                             in case xv of Right v -> (Right v, (M.insert s v f):fs)
                                           Left e -> (Left e, f:fs)
             _ -> (Left "Bad definition syntax", env)

sAdd :: Sval
sAdd = Sprim $ \vs env ->
  let (vs',env') = evalList vs env
      ret = do vals <- vs'
               case fst $ run (apply nump vals) env of
                 Right (Sbool True) -> return . foldl1 (+) $ map (\(Snum n) -> n) vals
                 Right _ -> Left $ "Bad type (expected numeric)"
                 Left err -> Left err
  in (fmap Snum ret, env')

apply :: Sval -> [Sval] -> Expr (Either String Sval)
apply (Sfunc ps body fe) vs = Expr $ \env ->
  if length ps /= length vs then (Left $ "Wrong number of arguments: " ++ show (length vs) ++ " for " ++ show (length ps), env)
    else let (vs', env') = evalList vs env
             ret = do vals <- vs'
                      let fe' = (M.fromList (ps `zip` vals)):fe -- push a new frame onto the stack
                      fst $ run (foldl1 (>>) $ map eval body) fe'
         in (ret,env')
apply (Sprim f) vs = Expr $ \env -> f vs env
apply (Ssym s) vs = Expr $ \env -> case envLookup s env of Nothing -> (Left $ "Unable to resolve symbol: " ++ s, env)
                                                           Just v  -> if v == Ssym s then (Left $ "Circular definition: " ++ s, env)
                                                                                     else run (apply v vs) env
apply (Slist l) vs = Expr $ \env -> let fn = fst $ run (eval $ Slist l) env
                                        ret = do fn' <- fn
                                                 return $ run (apply fn' vs) env
                                    in case ret of Right v -> v
                                                   Left err -> (Left err, env)
apply v _ = Expr $ \env -> (Left $ "Non-applicable value: " ++ show v, env)

toplevelBindings = [frame]
  where frame = M.fromList $
          [ ("lambda", lambda)
          , ("+", sAdd)
          , ("define", define)
          ]

{- TYPE PREDICATES -}

-- FIXME: these don't eval their arguments properly
--
stringp :: Sval
stringp = Sprim $ \vs e -> (return . Sbool $ all str vs, e)
  where str v = case v of Sstring _ -> True
                          _ -> False

nump :: Sval
nump = Sprim $ \vs e -> (return . Sbool $ all num vs, e)
  where num v = case v of Snum _ -> True
                          _ -> False

boolp :: Sval
boolp = Sprim $ \vs e -> (return . Sbool $ all bln vs, e)
  where bln v = case v of Sbool _ -> True
                          _ -> False

funcp :: Sval
funcp = Sprim $ \vs e -> (return . Sbool $ all fn vs, e)
  where fn v = case v of Sfunc _ _ _ -> True
                         _ -> False

