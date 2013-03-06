module Gretel.Persistence
( loadWorld
, dumpWorld
) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Gretel.World
import qualified Data.Map as M

createNodesTable :: String
createNodesTable = "CREATE TABLE `nodes` (`name` VARCHAR(255) PRIMARY KEY NOT NULL, `description` TEXT NOT NULL, `location` VARCHAR(255), FOREIGN KEY (`location`) REFERENCES `nodes` (`name`), CHECK (`name` <> `location`));"

createEdgesTable :: String
createEdgesTable = "CREATE TABLE `edges` (`origin` VARCHAR(255) NOT NULL, `direction` VARCHAR(255) NOT NULL, `destination` VARCHAR(255) NOT NULL, PRIMARY KEY (`origin`,`direction`), FOREIGN KEY (`origin`) REFERENCES `nodes` (`name`), FOREIGN KEY (`destination`) REFERENCES `nodes` (`name`));"

insertNode :: String
insertNode = "INSERT INTO `nodes` VALUES (?,?,?);" -- name, description, location

insertEdge :: String
insertEdge = "INSERT INTO `edges` VALUES (?,?,?);" -- origin, direction, destination

getNodes :: String
getNodes = "SELECT * FROM `nodes`;"

getEdges :: String
getEdges = "SELECT * FROM `edges`;"

dumpWorld :: World -> FilePath -> IO ()
dumpWorld w file = do
  conn <- connectSqlite3 file
  tbls <- getTables conn
  mapM_ (\t -> run conn ("DROP TABLE "++t) []) tbls

  _ <- run conn createNodesTable []
  _ <- run conn createEdgesTable []

  commit conn

  insn <- prepare conn insertNode
  insx <- prepare conn insertEdge

  let dump (Object n d l xs _) = do let dx (dir,dest) = execute insx [toSql n, toSql dir, toSql dest]
                                    _ <- execute insn [toSql n, toSql d, toSql l]
                                    mapM_ dx (M.toList xs)
  mapM_ dump $ elems w
  commit conn
  disconnect conn

loadWorld :: FilePath -> IO World
loadWorld file = do 
  conn <- connectSqlite3 file
  ns <- quickQuery' conn getNodes []
  es <- quickQuery' conn getEdges []
  disconnect conn

  let addNode ar = case ar of
        [n,d,l] -> let o = mkObject { name = fromSql n
                                    , description = fromSql d
                                    , location = fromSql l
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

