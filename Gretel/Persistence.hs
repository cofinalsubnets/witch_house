module Gretel.Persistence
( loadWorld
, saveWorld
, connect
, defaultWorld
, Connection
) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Gretel.World
import qualified Data.Map as M

createNodesTable :: String
createNodesTable = "CREATE TABLE `nodes` (`name` VARCHAR(255) PRIMARY KEY NOT NULL, `description` TEXT NOT NULL, `location` VARCHAR(255), `password` VARCHAR(255), `root` BOOLEAN NOT NULL, FOREIGN KEY (`location`) REFERENCES `nodes` (`name`), CHECK (`name` <> `location`));"

createEdgesTable :: String
createEdgesTable = "CREATE TABLE `edges` (`origin` VARCHAR(255) NOT NULL, `direction` VARCHAR(255) NOT NULL, `destination` VARCHAR(255) NOT NULL, PRIMARY KEY (`origin`,`direction`), FOREIGN KEY (`origin`) REFERENCES `nodes` (`name`), FOREIGN KEY (`destination`) REFERENCES `nodes` (`name`));"

insertNode :: String
insertNode = "INSERT INTO `nodes` VALUES (?,?,?,?,?);" -- name, description, location, password, root

insertEdge :: String
insertEdge = "INSERT INTO `edges` VALUES (?,?,?);" -- origin, direction, destination

getNodes :: String
getNodes = "SELECT * FROM `nodes`;"

getEdges :: String
getEdges = "SELECT * FROM `edges`;"


connect :: FilePath -> IO Connection
connect = connectSqlite3

saveWorld :: World -> Connection -> IO ()
saveWorld w conn = do
  tbls <- getTables conn
  mapM_ (\t -> run conn ("DROP TABLE "++t) []) tbls

  _ <- run conn createNodesTable []
  _ <- run conn createEdgesTable []

  commit conn

  insn <- prepare conn insertNode
  insx <- prepare conn insertEdge

  let dump (Object n d l xs _ pw r ) = do let dx (dir,dest) = execute insx [toSql n, toSql dir, toSql dest]
                                          _ <- execute insn [toSql n, toSql d, toSql l, toSql pw, toSql r]
                                          mapM_ dx (M.toList xs)
  mapM_ dump $ elems w
  commit conn


defaultWorld :: World
defaultWorld = set mkObject { name = "Root of the World", isRoot = True } mkWorld

loadWorld :: Connection -> IO World
loadWorld conn = do 
  tbls <- getTables conn
  if null tbls

    then do putStr "Creating DB... "
            saveWorld defaultWorld conn
            putStrLn "done!"
            return defaultWorld

    else do
      ns <- quickQuery' conn getNodes []
      es <- quickQuery' conn getEdges []

      let addNode ar = case ar of
            [n,d,l,p,r] -> let o = mkObject { name        = fromSql n
                                            , description = fromSql d
                                            , location    = fromSql l
                                            , password    = fromSql p
                                            , isRoot      = fromSql r
                                            }
                       in set o
            _ -> id

          addEdge ar = case ar of
            [orig,dir,dest] -> \w -> let obj = get' (fromSql orig) w
                                     in set obj { exits = M.insert (fromSql dir) (fromSql dest) (exits obj) } w
            _ -> id

          withNodes = foldr ($) mkWorld (map addNode ns)
          withEdges = foldr ($) withNodes (map addEdge es)
      
      return withEdges

