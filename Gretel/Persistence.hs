module Gretel.Persistence
( createDB
, dumpWorld
) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Gretel.World
import Data.Monoid

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

connect :: FilePath -> IO Connection
connect = connectSqlite3

createDB :: FilePath -> IO ()
createDB file = do
  conn <- connectSqlite3 file
  run conn createNodesTable []
  run conn createEdgesTable []
  commit conn
  disconnect conn

dumpWorld :: World -> FilePath -> IO ()
dumpWorld w file = do
  conn <- connectSqlite3 file
  insn <- prepare conn insertNode
  insx <- prepare conn insertEdge

  let toObj o = (getName o w, getDesc o w, getLoc o w, getExits' o w)
      os = map toObj (getObjs w)
      dump (n,d,l,xs) = do let dx (dir,dest) = execute insx [toSql n, toSql dir, toSql dest]
                           execute insn [toSql n, toSql d, toSql l]
                           mapM_ dx xs
  mapM_ dump os
  disconnect conn

loadWorld :: FilePath -> IO World
loadWorld file = do 
  conn <- connectSqlite3 file
  ns <- quickQuery' conn getNodes []
  es <- quickQuery' conn getEdges []
  disconnect conn

  let addNode [n,d,l] = do let n' = fromSql n
                           addObj' n'
                           setDesc' n' (fromSql d)
                           case fromSql l of
                             Nothing -> return True
                             Just l' -> setLoc' n' l'

      addEdge [orig,dir,dest] = do let orig' = fromSql orig
                                   addExit' orig' (fromSql dir) (fromSql dest)

      addNodes = mconcat $ map addNode ns
      addEdges = mconcat $ map addEdge es
      w = mkWorld
  
  return $ execWorld (addNodes >> addEdges) w

