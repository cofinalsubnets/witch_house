module WitchHouse.Persistence
( saveWorld
, connect
, disconnect
, loadWorld
) where

import Database.HDBC
import Database.HDBC.Sqlite3
import WitchHouse.World hiding (run)
import Data.Unique
import qualified Data.Map as M
import Data.Maybe
import Data.List hiding (find)
import qualified Data.List as List (find)
import WitchHouse.Types hiding (run)
import WitchHouse.Wisp (evalWisp)

createNodesTable :: String
createNodesTable = "CREATE TABLE `nodes` (`id` INTEGER PRIMARY KEY, `name` VARCHAR(255) NOT NULL, `description` TEXT NOT NULL, `location` INTEGER, `password` VARCHAR(255), `root` BOOLEAN NOT NULL, FOREIGN KEY (`location`) REFERENCES `nodes` (`id`), CHECK (`id` <> `location`));"

createEdgesTable :: String
createEdgesTable = "CREATE TABLE `edges` (`origin` INTEGER NOT NULL, `direction` VARCHAR(255) NOT NULL, `destination` INTEGER NOT NULL, PRIMARY KEY (`origin`,`direction`), FOREIGN KEY (`origin`) REFERENCES `nodes` (`id`), FOREIGN KEY (`destination`) REFERENCES `nodes` (`id`));"

createWispsTable :: String
createWispsTable = "CREATE TABLE `wisps` (`node` INTEGER NOT NULL, `name` VARCHAR(255) NOT NULL, `wisp` TEXT NOT NULL, PRIMARY KEY (`node`, `name`), FOREIGN KEY (`node`) REFERENCES `nodes` (`id`));"

insertNode :: String
insertNode = "INSERT INTO `nodes` VALUES (?,?,?,?,?,?);" -- id, name, description, location, password, root

insertEdge :: String
insertEdge = "INSERT INTO `edges` VALUES (?,?,?);" -- origin, direction, destination

insertWisp :: String
insertWisp = "INSERT INTO `wisps` VALUES (?,?,?);" -- node, name, wisp

getNodes :: String
getNodes = "SELECT * FROM `nodes`;"

getEdges :: String
getEdges = "SELECT * FROM `edges`;"

getWisps :: String
getWisps = "SELECT * FROM `wisps`;"

connect :: FilePath -> IO Connection
connect = connectSqlite3

saveWorld :: World -> Connection -> IO ()
saveWorld w conn = do
  tbls <- getTables conn
  mapM_ (\t -> run conn ("DROP TABLE "++t) []) tbls

  _ <- run conn createNodesTable []
  _ <- run conn createEdgesTable []
  _ <- run conn createWispsTable []

  commit conn

  insn <- prepare conn insertNode
  insx <- prepare conn insertEdge
  insw <- prepare conn insertWisp

  let dump l o = do let oid = hashUnique . objId $ o
                        dx (dir,dest) = execute insx [toSql oid, toSql dir, toSql (hashUnique dest)]
                        dw (nm,wisp)  = execute insw [toSql oid, toSql nm, toSql (show wisp)]
                        func sv = case sv of { Sfunc _ _ _ -> True; _ -> False }
                    _ <- execute insn [toSql oid, toSql (name o), toSql (description o), toSql l, toSql (password o), toSql (start o)]
                    mapM_ dx (M.toList $ exits o)
                    mapM_ dw (filter (func . snd) . M.toList . fst $ bindings o M.! 0)
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
  ws <- quickQuery' conn getWisps []

  let mapNode ar = case ar of
        [i,n,d,l,p,r] -> do o <- mkObj
                            let l' = case fromSql l :: Maybe Int of Just k -> k
                                                                    Nothing -> -1
                            return $ (fromSql i, l', o { name        = fromSql n
                                                       , description = fromSql d
                                                       , password    = fromSql p
                                                       , start       = fromSql r
                                                       })
        _ -> error "loadWorld: malformed node"

      mapEdge ar = case ar of
        [orig,dir,dest] -> (fromSql orig, fromSql dir, fromSql dest)
        _ -> error "loadWorld: malformed edge"

      mapWisp :: [SqlValue] -> (Int,String,String)
      mapWisp ar = case ar of
        [nid,nm,wsp] -> (fromSql nid, fromSql nm, fromSql wsp)
        _ -> error "loadWorld: malformed wisp"

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
      addWisp js (nid,nm,wsp) = let ([(_,loc,t)],os) = partition (\(i,_,_) -> i==nid) js
                                    wstr = concat ["(define ",nm," ",wsp,")"]
                                    r = evalWisp wstr (t,[])
                                in case r of Right (t',_) -> (nid,loc,t'):os
                                             Left _ -> ((nid,loc,t):os)
      wisps = map mapWisp ws

      edges = map mapEdge es
  nodes <- mapM mapNode ns

  return . mkWorld . consolidate $ foldl addEdge (foldl addWisp nodes wisps) edges

