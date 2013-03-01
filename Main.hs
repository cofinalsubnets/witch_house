import Gretel.World
import Gretel.Command
import Data.Maybe
import System.IO

type Interactor = [String] -> [String]

(<.<) :: ([a] -> [b]) -> (c -> a) -> ([c] -> [b])
i <.< f = i . map f

testRun = runWorld testWorld <.< fromMaybe huh <.< parseCommand rootMap <.< prefix "feivel"
testRunIO = runIO stdin stdout testRun

prefix p s = p ++ " " ++ s

hGetCommands :: Handle -> IO [String]
hGetCommands h = hGetContents h >>= return . lines

getCommands :: IO [String]
getCommands = hGetCommands stdin

test = let runner = runWorld testWorld <.< fromMaybe ioHuh <.< parseCommand ioMap <.< prefix "feivel"
  in getCommands >>= mapM_ (>>=putStrLn) . runner

runIO :: Handle -> Handle -> Interactor -> IO ()
runIO i o r = hGetCommands i >>= mapM_ (hPutStrLn o) . r

main = test

