{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Data.String (fromString)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as WaiParse
import Network.HTTP.Types.URI (parseSimpleQuery)
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Types.Method as M
import qualified Network.Wai.Application.Static as Static
import Data.Maybe (fromJust)
import WaiAppStatic.Types (toPieces)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Text.Read (readMaybe)

import Control.Exception (SomeException(..),catch,displayException)
import Data.Pool (Pool,createPool,withResource)

import qualified Database.MySQL.Simple as MySQL
import qualified Data.Aeson as AE

import Data
import MySQL
import JSON


main :: IO ()
main = do
  host:port:_ <- getArgs
  cp <- createPool connect close 10 10 10
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
    Warp.defaultSettings
    ) $ routerApp cp
  where
    connect :: IO MySQL.Connection
    connect = MySQL.connect MySQL.defaultConnectInfo {
       MySQL.connectHost = "localhost"
      ,MySQL.connectUser = "wai_exam_admin"
      ,MySQL.connectPassword = "abcd1234"
      ,MySQL.connectDatabase = "wai_exam"
      }
    close = MySQL.close



routerApp :: Pool MySQL.Connection -> Wai.Application
routerApp cp req respond = case Wai.pathInfo req of
  "v1" : _ -> (v1App cp req respond) `catch` onException -- /v1{var}
  _ -> staticApp req respond -- static html/js/css files
  where
    onException :: SomeException -> IO Wai.ResponseReceived
    onException (SomeException e) = respond $ responseNG 5000 "unknown error"
--    onException (SomeException e) = respond $ responseNG 5000 $ displayException e


v1App :: Pool MySQL.Connection -> Wai.Application
v1App cp req respond = case Wai.pathInfo req of
  _ : "genre"  : _ -> genreApp cp req respond -- /v1/genre{var}
  _ : "tag"    : _ -> tagApp cp req respond   -- /v1/tag{var}
  _ : "look"   : _ -> lookApp cp req respond  -- /v1/look{var}
  _ -> staticApp req respond -- static html/js/css files


{-
curl -X POST \
  -d title="ジャンル4" \
  http://localhost:9999/v1/genre | jq .

curl -X DELETE \
  http://localhost:9999/v1/genre/4 | jq .

curl -X GET \
  http://localhost:9999/v1/genre | jq .

curl -X GET \
  http://localhost:9999/v1/genre/look | jq .

-}
genreApp :: Pool MySQL.Connection -> Wai.Application
genreApp cp req respond = case (M.parseMethod (Wai.requestMethod req), Wai.pathInfo req) of
  
  -- POST /v1/genre
  (Right M.POST, [_,_]) -> do
    ps <- parseForm req
    case lookupParam "title" ps of
      Nothing -> respond $ responseNG 5101 "invalid title"
      Just t -> do
        bs <- AE.encode <$> (withResource cp $ \conn -> create_genre conn t)
        respond $ response200 bs
             
  -- DELETE /v1/genre/{id}
  (Right M.DELETE, [_,_,id]) ->
    case readMaybe (T.unpack id) of
      Nothing -> respond $ responseNG 5102 "invalid id"
      Just id -> do
        withResource cp $ \conn -> delete_genre conn id
        respond responseOK
              
  -- GET /v1/genre
  (Right M.GET, [_,_]) -> do
    bs <- AE.encode <$> (withResource cp $ \conn -> select_genre conn)
    respond $ response200 bs
    
  -- GET /v1/genre/look
  (Right M.GET, [_,_,"look"]) -> do
    bs <- AE.encode <$> (withResource cp $ \conn -> select_look_by_genre conn)
    respond $ response200 bs

  (_,_) -> respond $ responseNG 5100 "not implemented"


{-
curl -X POST \
  -d title="タグ4" \
  http://localhost:9999/v1/tag | jq .

curl -X DELETE \
  http://localhost:9999/v1/tag/4 | jq .

curl -X GET \
  http://localhost:9999/v1/tag | jq .

curl -X GET \
  http://localhost:9999/v1/tag/look | jq .

-}
tagApp :: Pool MySQL.Connection -> Wai.Application
tagApp cp req respond = case (M.parseMethod (Wai.requestMethod req), Wai.pathInfo req) of
  
  -- POST /v1/tag
  (Right M.POST, [_,_]) -> do
    ps <- parseForm req
    case lookupParam "title" ps of
      Nothing -> respond $ responseNG 5201 "invalid title"
      Just t -> do
        bs <- AE.encode <$> (withResource cp $ \conn -> create_tag conn t)
        respond $ response200 bs
             
  -- DELETE /v1/tag/{id}
  (Right M.DELETE, [_,_,id]) ->
    case readMaybe (T.unpack id) of
      Nothing -> respond $ responseNG 5202 "invalid id"
      Just id -> do
        withResource cp $ \conn -> delete_tag conn id
        respond responseOK
              
  -- GET /v1/tag
  (Right M.GET, [_,_]) -> do
    bs <- AE.encode <$> (withResource cp $ \conn -> select_tag conn)
    respond $ response200 bs
    
  -- GET /v1/tag/look
  (Right M.GET, [_,_,"look"]) -> do
    bs <- AE.encode <$> (withResource cp $ \conn -> select_look_by_tag conn)
    respond $ response200 bs

  (_,_) -> respond $ responseNG 5200 "not implemented"


{-
curl -X POST \
  -d title="コーデ6タイトル" \
  -d description="コーデ6説明" \
  -d genre_id=2 \
  -d tag_id="2 3" \
  http://localhost:9999/v1/look | jq .

curl -X PUT \
  -d title="コーデ6タイトル改" \
  -d description="コーデ6説明改" \
  -d genre_id=3 \
  -d tag_id="1 2 4" \
  http://localhost:9999/v1/look/6 | jq .

curl -X DELETE \
  http://localhost:9999/v1/look/6 | jq .

curl -X GET \
  http://localhost:9999/v1/look/6 | jq .

curl -X GET \
  "http://localhost:9999/v1/look" | jq .

curl -X GET \
  "http://localhost:9999/v1/look?title=%6%" | jq .

curl -X GET \
  "http://localhost:9999/v1/look?genre_id=2" | jq .

curl -X GET \
  "http://localhost:9999/v1/look?tag_id=3" | jq .

curl -X GET \
  "http://localhost:9999/v1/look?genre_id=2&tag_id=3" | jq .

-}
lookApp :: Pool MySQL.Connection -> Wai.Application
lookApp cp req respond = case (M.parseMethod (Wai.requestMethod req), Wai.pathInfo req) of

  -- POST /v1/look
  (Right M.POST, [_,_]) -> do
    ps <- parseForm req
    case validateParam ps of
      Nothing -> respond $ responseNG 5301 "invalid params"
      Just (t,d,gid,tids) -> do
        bs <- AE.encode <$> (withResource cp $ \conn -> create_look conn t d gid tids)
        respond $ response200 bs
        
  -- PUT /v1/look/{id}
  (Right M.PUT, [_,_,id]) -> do
    ps <- parseForm req
    case (\x y ->(x,y)) <$> readMaybe (T.unpack id) <*> validateParam ps of
      Nothing -> respond $ responseNG 5302 "invalid params"
      Just (id,(t,d,gid,tids)) -> do
        ml <- withResource cp $ \conn -> update_look conn id t d gid tids
        case ml of
          Nothing -> respond $ responseNG 5303 "not found"
          Just l  -> respond $ response200 $ AE.encode l
    
  -- DELETE /v1/look/{id}
  (Right M.DELETE, [_,_,id]) ->
    case readMaybe (T.unpack id) of
      Nothing -> respond $ responseNG 5304 "invalid id"
      Just id -> do
        withResource cp $ \conn -> delete_look conn id
        respond responseOK
    
  -- GET /v1/look
  (Right M.GET, [_,_]) -> do
    let (t,d,gid,tid) = parseQuery query
    bs <- AE.encode <$> (withResource cp $ \conn -> select_look conn t d gid tid)
    respond $ response200 bs
    
  -- GET /v1/look/{id}
  (Right M.GET, [_,_,id]) ->
    case readMaybe (T.unpack id) of
      Nothing -> respond $ responseNG 5305 "invalid id"
      Just id -> do
        ml <- withResource cp $ \conn -> get_look conn id
        case ml of
          Nothing -> respond $ responseNG 5306 "not found"
          Just l  -> respond $ response200 $ AE.encode l

  (_,_) -> respond $ responseNG 5300 "not implemented"

  where
    query = parseSimpleQuery $ Wai.rawQueryString req

    validateParam :: [WaiParse.Param] -> Maybe (Title,Description,GenreId,[TagId])
    validateParam ps = do
      t    <- lookupParam "title" ps
      d    <- lookupParam "description" ps
      gid  <- readMaybe =<< lookupParam "genre_id" ps
      tids <- mapM readMaybe . words =<< lookupParam "tag_id" ps
      return (t,d,gid,tids)

    parseQuery :: [WaiParse.Param] -> (Maybe Title,Maybe Description,Maybe GenreId, Maybe TagId)
    parseQuery qs =
      let
        t   = lookupParam "title" qs
        d   = lookupParam "description" qs
        gid = readMaybe =<< lookupParam "genre_id" qs
        tid = readMaybe =<< lookupParam "tag_id" qs
      in (t,d,gid,tid)




response200 :: LBS.ByteString -> Wai.Response
response200 = Wai.responseLBS H.status200 [("Content-Type","application/json")] 

responseOK :: Wai.Response
responseOK = response200 $ AE.encode OK

responseNG :: Int -> String -> Wai.Response
responseNG code msg =
  Wai.responseLBS H.status500 [("Content-Type","application/json")] $ AE.encode $ NG code msg




parseForm :: Wai.Request -> IO [WaiParse.Param]
parseForm req =
  fst <$> WaiParse.parseRequestBodyEx
    WaiParse.defaultParseRequestBodyOptions WaiParse.lbsBackEnd req

lookupParam :: BS.ByteString -> [WaiParse.Param] -> Maybe String
lookupParam key ps = T.unpack . decodeUtf8 <$> lookup key ps




staticApp :: Wai.Application
staticApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.defaultWebAppSettings "static"
    indices = fromJust $ toPieces ["main.html"] -- default content

