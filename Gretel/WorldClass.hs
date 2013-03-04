{-# LANGUAGE MultiParamTypeClasses, Rank2Types #-}

type WT w = World w => w -> (Bool,w)

class (Ord a, World w) => Key w a
class World w => Desc w a
class (Ord a, World w) => Edge w a

class World a where

  goesTo :: Key a k => k -> k -> WT a

  from :: (Edge a e, Key a k) => e -> k -> a -> Maybe k

  contents :: (Key a k) => k -> a -> [k]

  exits :: (Key a k, Edge a e) => k -> a -> [e]

  location :: Key a k => k -> a -> Maybe k

  goes :: (Key a k, Edge a e) => k -> e -> WT a
  goes k e = \w -> case e `from` k $ w of Nothing -> (False,w)
                                          Just d  -> k `goesTo` d $ w
  name :: Key a k => k -> a -> Maybe String
  description :: Key a k => k -> a -> Maybe String


  takes :: Key a k => k -> k -> WT a
  k1 `takes` k2 = \w ->
    if location k1 w == location k2 w
      then k2 `goesTo` k1 $ w
      else (False,w)

  enters :: Key a k => k -> k -> WT a
  k1 `enters` k2 = k2 `takes` k1

  drops :: Key a k => k -> k -> WT a
  k1 `drops` k2 = \w ->
    if location k2 w == Just k1
      then case location k1 w of
        Nothing -> (False,w)
        Just l  -> k2 `goesTo` l
      else (False,w)

  leaves :: Key a k => k -> k -> WT a
  k1 `leaves` k2 = k2 `drops` k1

  adjoins :: (Key a k, Edge a e) => k -> k -> e -> WT a

  deadends :: (Key a k, Edge a e) => k -> e -> WT a

  makes :: Key a k => k -> k -> WT a

  describes :: (Desc a d, Key a k) => d -> k -> WT a


