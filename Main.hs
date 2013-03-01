import Gretel.World
import Gretel.Command
import Gretel.Types
import System.IO

type Interactor = [String] -> [IO String]

runWorld :: World -> [WorldTransformer a] -> [a]
runWorld w (t:ts) = let (r,w') = t w in r:(runWorld w' ts)
runWorld _ [] = []

(<.<) :: ([a] -> [b]) -> (c -> a) -> ([c] -> [b])
i <.< f = i . map f

testRun = runWorld testWorld <.< parseCommand rootMap <.< prefix "feivel"
testRunIO = runIO stdin stdout testRun

prefix p s = p ++ " " ++ s

hGetCommands :: Handle -> IO [String]
hGetCommands h = hGetContents h >>= return . lines

getCommands :: IO [String]
getCommands = hGetCommands stdin


runIO :: Handle -> Handle -> Interactor -> IO ()
runIO i o r = hGetCommands i >>= mapM_ display . r
  where display s = s >>= hPutStrLn o

main = testRunIO

