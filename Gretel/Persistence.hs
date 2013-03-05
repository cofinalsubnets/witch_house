module Gretel.Persistence
( loadWorld
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

  let toObj o = (getName o w, getDesc o w, getLoc o w, getExits' o w)
      os = map toObj (getObjs w)
      dump (n,d,l,xs) = do let dx (dir,dest) = execute insx [toSql n, toSql dir, toSql dest]
                           _ <- execute insn [toSql n, toSql d, toSql l]
                           mapM_ dx xs
  mapM_ dump os
  commit conn
  disconnect conn

loadWorld :: FilePath -> IO World
loadWorld file = do 
  conn <- connectSqlite3 file
  ns <- quickQuery' conn getNodes []
  es <- quickQuery' conn getEdges []
  disconnect conn

  let addNode ar = case ar of
        [n,d,l] -> do let n' = fromSql n
                      _ <- WS $ addObj n'
                      _ <- WS $ setDesc n' (fromSql d)
                      case fromSql l of
                        Nothing -> return True
                        Just l' -> WS $ setLoc n' l'
        _ -> return True

      addEdge ar = case ar of
        [orig,dir,dest] -> do let orig' = fromSql orig
                              WS $ adjoins orig' (fromSql dir) (fromSql dest)
        _ -> return True

      addNodes = mconcat $ map addNode ns
      addEdges = mconcat $ map addEdge es
      w = mkWorld
  
  return $ execWorld (addNodes >> addEdges) w
