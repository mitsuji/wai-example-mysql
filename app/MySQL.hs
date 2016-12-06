{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

module MySQL (create_genre
             ,delete_genre
             ,create_tag
             ,delete_tag
             ,create_look
             ,update_look
             ,delete_look
             ,select_genre
             ,select_tag
             ,get_look
             ,select_look
             ,select_look_by_genre
             ,select_look_by_tag
             ) where

import Database.MySQL.Simple
import Database.MySQL.Simple.Result
import Database.MySQL.Simple.QueryResults

import Data.Maybe (fromJust)
import Data.List (groupBy)
import Text.RawString.QQ (r)
import qualified Data.ByteString.Builder as BD
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map

import Data


instance QueryResults Look where
  convertResults [fa,fb,fc,fd,fe,ff,fg] [va,vb,vc,vd,ve,vf,vg] =
    Look a b c d e (Genre' $ Genre f g []) []
        where !a = convert fa va
              !b = convert fb vb
              !c = convert fc vc
              !d = convert fd vd
              !e = convert fe ve
              !f = convert ff vf
              !g = convert fg vg
  convertResults fs vs  = convertError fs vs 7

instance QueryResults Tag' where
  convertResults [fa,fb] [va,vb] = Tag' $ Tag a b []
        where !a = convert fa va
              !b = convert fb vb
  convertResults fs vs  = convertError fs vs 2

instance QueryResults Genre' where
  convertResults [fa,fb] [va,vb] = Genre' $ Genre a b []
        where !a = convert fa va
              !b = convert fb vb
  convertResults fs vs  = convertError fs vs 2

data LookHasTag = LookHasTag LookId Tag'
instance QueryResults LookHasTag where
  convertResults [fa,fb,fc] [va,vb,vc] = LookHasTag a (Tag' $ Tag b c [])
    where !a = convert fa va
          !b = convert fb vb
          !c = convert fc vc
  convertResults fs vs  = convertError fs vs 3
  



get_last_id :: Connection -> IO Int
get_last_id conn = do
  [Only id] <- query_ conn "SELECT LAST_INSERT_ID()"
  return id




create_genre :: Connection -> Title -> IO Genre
create_genre conn t = do
  execute conn
    "INSERT INTO genre (genre_title) VALUES (?)" (Only t)
  id' <- get_last_id conn
  return $ Genre id' t []
  
delete_genre :: Connection -> GenreId -> IO ()
delete_genre conn id = do
  execute conn
    "DELETE FROM genre WHERE genre_id = ?" (Only id)
  return ()

create_tag :: Connection -> Title -> IO Tag
create_tag conn t = do
  execute conn
    "INSERT INTO tag (tag_title) VALUES (?)" (Only t)
  id' <- get_last_id conn
  return $ Tag id' t []

delete_tag :: Connection -> TagId -> IO ()
delete_tag conn id = do
  execute conn
    "DELETE FROM tag WHERE tag_id = ?" (Only id)
  return ()

create_look :: Connection
               -> Title -> Description -> GenreId -> [TagId] -> IO Look
create_look conn t d gid tids = do
  id' <- withTransaction conn $ do
    execute conn [r|
      INSERT INTO look (
         look_create_dt
        ,look_update_dt
        ,look_title
        ,look_description
        ,look_genre_id
      )
      VALUES (NOW(),NOW(),?,?,?)
    |] (t,d,gid)
    id' <- get_last_id conn
    set_lht conn id' tids
    return id'
  fromJust <$> get_look conn id'

update_look :: Connection -> LookId
               -> Title -> Description -> GenreId -> [TagId] -> IO (Maybe Look)
update_look conn id t d gid tids = do
  [Only count] <- query conn "SELECT COUNT(*) FROM look WHERE look_id = ?" (Only id)
  if count == (0::Int)
    then return Nothing
    else do
      withTransaction conn $ do
        execute conn [r|
          UPDATE look SET
             look_update_dt = NOW()
            ,look_title = ?
            ,look_description = ?
            ,look_genre_id = ?
          WHERE look_id = ?
        |] (t,d,gid,id)
        set_lht conn id tids
      get_look conn id

delete_look :: Connection -> LookId -> IO ()
delete_look conn id =
  withTransaction conn $ do
    execute conn
      "DELETE FROM look_has_tag WHERE lht_look_id = ?" (Only id)
    execute conn
      "DELETE FROM look WHERE look_id = ?" (Only id)
    return ()

set_lht ::Connection -> LookId -> [TagId] -> IO ()
set_lht conn id tids = do
  execute conn
    "DELETE FROM look_has_tag WHERE lht_look_id = ?"
    (Only id)
  executeMany conn
    "INSERT INTO look_has_tag (lht_look_id,lht_tag_id) VALUES (?,?)"
    (map (\x->(id,x)) tids)
  return ()
  



select_genre :: Connection -> IO [Genre']
select_genre conn =
  query_ conn [r|
    SELECT
       genre_id
      ,genre_title
    FROM genre
    ORDER BY genre_id
  |]
  
select_tag :: Connection -> IO [Tag']
select_tag conn =
  query_ conn [r|
    SELECT
       tag_id
      ,tag_title
    FROM tag
    ORDER BY tag_id
  |]



  
merge_tags :: Map.Map LookId [Tag'] -> [Look] -> [Look]
merge_tags tagsMap ls =
  map ((\l -> case Map.lookup (lookId l) tagsMap of
           Just tags -> l {lookTags=tags}
           Nothing   -> l
       )) ls
  
select_tag_for_looks :: Connection -> [Look] -> IO (Map.Map LookId [Tag'])
select_tag_for_looks conn ls = do
  xs <- query conn [r|
    SELECT
       look_id
      ,tag_id
      ,tag_title
    FROM look
      INNER JOIN look_has_tag ON look_id = lht_look_id
      INNER JOIN tag ON lht_tag_id = tag_id
    WHERE look_id IN ?
    ORDER BY look_id, tag_id
  |] $ Only $ In $ map lookId ls
  return $ Map.fromList
         $ map (\xxs@((LookHasTag id _):_) -> (id,map (\(LookHasTag _ tags) -> tags) xxs))
         $ groupBy (\(LookHasTag id1 _) (LookHasTag id2 _) -> id1 == id2) xs




get_look :: Connection -> LookId -> IO (Maybe Look)
get_look conn id = do
  ls <- query conn [r|
    SELECT
       look_id
      ,look_create_dt
      ,look_update_dt
      ,look_title
      ,look_description
      ,genre_id
      ,genre_title
    FROM look
      LEFT OUTER JOIN genre ON look_genre_id = genre_id
    WHERE look_id = ?
  |] (Only id)

  case ls of
    [] -> return Nothing
    _  -> do
          tags <- select_tag_for_looks conn ls
          return $ Just $ head $ merge_tags tags ls


-- [TODO] sanitize param of LIKE phrase 
select_look :: Connection -> Maybe Title -> Maybe Description -> Maybe GenreId -> Maybe TagId -> IO [Look]
select_look conn mt md mgid mtid = do

  (ft,t) <- case mt of
    Nothing -> return (1::Int,"%")
    Just t ->  return (0::Int,t)
    
  (fd,d) <- case md of
    Nothing -> return (1::Int,"%")
    Just d ->  return (0::Int,d)

  (fgid,gid) <- case mgid of
    Nothing ->  return (1::Int,0)
    Just gid -> return (0::Int,gid)

  (ftid,tid) <- case mtid of
    Nothing ->  return (1::Int,0)
    Just tid -> return (0::Int,tid)

  ls <- query conn [r|
    SELECT
       look_id
      ,look_create_dt
      ,look_update_dt
      ,look_title
      ,look_description
      ,genre_id
      ,genre_title
    FROM look
      LEFT OUTER JOIN genre ON look_genre_id = genre_id
    WHERE 1 = 1
      AND (1=? OR look_title LIKE ?)
      AND (1=? OR look_description LIKE ?)
      AND (1=? OR look_genre_id = ?)
      AND (1=? OR look_id IN
                  (SELECT lht_look_id FROM look_has_tag WHERE lht_tag_id =?))
    ORDER BY look_id
  |](ft,t,fd,d,fgid,gid,ftid,tid)

  tags <- select_tag_for_looks conn ls
  return $ merge_tags tags ls
                         







data GenreHasLook = GenreHasLook Genre Look
instance QueryResults GenreHasLook where
  convertResults [fa,fb,fc,fd,fe,ff,fg] [va,vb,vc,vd,ve,vf,vg] =
    GenreHasLook (Genre f g []) (Look a b c d e (Genre' $ Genre f g []) []) 
    where !a = convert fa va
          !b = convert fb vb
          !c = convert fc vc
          !d = convert fd vd
          !e = convert fe ve
          !f = convert ff vf
          !g = convert fg vg
  convertResults fs vs  = convertError fs vs 7
  
data TagHasLook = TagHasLook Tag Look
instance QueryResults TagHasLook where
  convertResults [fa,fb,fc,fd,fe,ff,fg,fh,fi] [va,vb,vc,vd,ve,vf,vg,vh,vi] =
    TagHasLook (Tag a b []) (Look c d e f g (Genre' $ Genre h i []) []) 
    where !a = convert fa va
          !b = convert fb vb
          !c = convert fc vc
          !d = convert fd vd
          !e = convert fe ve
          !f = convert ff vf
          !g = convert fg vg
          !h = convert fh vh
          !i = convert fi vi
  convertResults fs vs  = convertError fs vs 9


select_look_by_genre :: Connection -> IO [Genre]
select_look_by_genre conn = do
  xs <- query_ conn [r|
    SELECT
       look_id
      ,look_create_dt
      ,look_update_dt
      ,look_title
      ,look_description
      ,genre_id
      ,genre_title
    FROM look
      LEFT OUTER JOIN genre ON look_genre_id = genre_id
    ORDER BY genre_id, look_id
  |]

  tags <- select_tag_for_looks conn $ map (\(GenreHasLook _ l) -> l) xs
  return $ map (\(Genre id t ls) -> Genre id t (merge_tags tags ls))
         $ map (\xxs@((GenreHasLook (Genre id t []) _):_) -> Genre id t (map (\(GenreHasLook _ l) -> l) xxs))
         $ groupBy (\(GenreHasLook g1 _) (GenreHasLook g2 _) -> g1 == g2) xs


select_look_by_tag :: Connection -> IO [Tag]
select_look_by_tag conn = do
  xs <- query_ conn [r|
    SELECT
       tag_id
      ,tag_title
      ,look_id
      ,look_create_dt
      ,look_update_dt
      ,look_title
      ,look_description
      ,genre_id
      ,genre_title
    FROM look
      LEFT OUTER JOIN genre ON look_genre_id = genre_id
      LEFT OUTER JOIN look_has_tag ON look_id = lht_look_id
      INNER JOIN tag ON lht_tag_id = tag_id
    ORDER BY tag_id, look_id
  |]

  tags <- select_tag_for_looks conn $ map (\(TagHasLook _ l) ->l) xs
  return $ map (\(Tag id t ls) -> Tag id t (merge_tags tags ls))
         $ map (\xxs@((TagHasLook (Tag id t []) _):_) -> Tag id t (map (\(TagHasLook _ l) -> l) xxs))
         $ groupBy (\(TagHasLook t1 _) (TagHasLook t2 _) -> t1 == t2) xs

