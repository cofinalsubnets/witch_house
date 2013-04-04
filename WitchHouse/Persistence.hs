{-# LANGUAGE TupleSections #-}
module WitchHouse.Persistence
( saveWorld
, connect
, disconnect
, loadWorld
) where

import Database.HDBC
import Database.HDBC.Sqlite3
import WitchHouse.World
import qualified Data.Map as M
import Data.Maybe
import Data.List hiding (find)
import qualified Data.List as List (find)
import WitchHouse.Types
import Data.ByteString.Char8 (pack)
import WitchHouse.Wisp (bind)

createNodesTable :: String
createNodesTable = "CREATE TABLE `nodes` (`id` INTEGER PRIMARY KEY, `name` VARCHAR(255) NOT NULL, `description` TEXT NOT NULL, `location` INTEGER, `password` VARCHAR(255), `root` BOOLEAN NOT NULL, FOREIGN KEY (`location`) REFERENCES `nodes` (`id`), CHECK (`id` <> `location`));"

createEdgesTable :: String
createEdgesTable = "CREATE TABLE `edges` (`origin` INTEGER NOT NULL, `direction` VARCHAR(255) NOT NULL, `destination` INTEGER NOT NULL, PRIMARY KEY (`origin`,`direction`), FOREIGN KEY (`origin`) REFERENCES `nodes` (`id`), FOREIGN KEY (`destination`) REFERENCES `nodes` (`id`));"

insertNode :: String
insertNode = "INSERT INTO `nodes` VALUES (?,?,?,?,?,?);" -- id, name, description, location, password, root

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

  let dump l o = do let oid = objId o
                        dx (dir,dest) = execute insx [toSql oid, toSql dir, toSql dest]
                    _ <- execute insn [toSql oid, toSql (name o), toSql (description o), toSql l, toSql (password o), toSql (start o)]

                    mapM_ dx (M.toList $ exits o)
                    mapM_ (dump $ Just oid) (contents o)

  dump Nothing . focus . findRoot $ w
  commit conn

findRoot :: World -> World
findRoot w@(_,[]) = w
findRoot w = findRoot $ zUp' w


loadWorld :: Connection -> IO World
loadWorld conn = do 
  ns <- quickQuery' conn getNodes []
  es <- quickQuery' conn getEdges []

  let mapNode ar = case ar of
        [i,n,d,l,p,r] -> do o <- mkObj
                            let l' = case fromSql l :: Maybe Int of Just k -> k
                                                                    Nothing -> -1
                                f = objId o
                            bind f (pack "*name*")     . Str $ fromSql n
                            bind f (pack "*password*") . Str $ fromSql p
                            bind f (pack "*desc*")     . Str $ fromSql d
                            return $ (fromSql i, l', o{start = fromSql r})
        _ -> error "loadWorld: malformed node"

      mapEdge ar = case ar of
        [orig,dir,dest] -> (fromSql orig, fromSql dir, fromSql dest)
        _ -> error "loadWorld: malformed edge"

      consolidate :: [(Int,Int,Obj)] -> Obj
      consolidate [(_,_,n)] = n
      consolidate os = let (zs,is) = partition (\(i,_,_) -> 0 == length (filter (\(_,l,_) -> l==i) os)) os
                           ns' = foldl insNode is zs
                           insNode cs (_,l,n) = let ([(di,dl,d)],js) = partition (\(i,_,_) -> i==l) cs
                                                in (di,dl,d{contents = n:(contents d)}):js
                       in consolidate ns'
      addEdge js (o,dir,d) = let ([(_,l,orig)],os) = partition (\(i,_,_) -> i==o) js
                                 (_,_,dest) = fromJust $ List.find (\(i,_,_) -> i==d) js
                                   
                             in (o,l,orig{exits = M.insert dir (objId dest) (exits orig)}):os
      edges = map mapEdge es

  nodes <- mapM mapNode ns

  return . (,[]) . consolidate $ foldl addEdge nodes edges

