{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Database.MySQL.Simple as MySQL
import qualified Database.MySQL.Simple.Result as MySQLR
import qualified Database.MySQL.Simple.QueryResults as MySQLQR
import Control.Monad(forM_)


data IdName = IdName Int String

instance MySQLQR.QueryResults IdName where
  convertResults [fId,fName] [vId,vName] = IdName id name
    where !id   = MySQLR.convert fId   vId
          !name = MySQLR.convert fName vName
  convertResults fs vs = MySQLQR.convertError fs vs 2



data User = User Int String Int Float

instance MySQLQR.QueryResults User where
  convertResults
    [fId,fName,fAge,fHeight] [vId,vName,vAge,vHeight] =
      User id name age height
    where !id     = MySQLR.convert fId     vId
          !name   = MySQLR.convert fName   vName
          !age    = MySQLR.convert fAge    vAge
          !height = MySQLR.convert fHeight vHeight
  convertResults fs vs = MySQLQR.convertError fs vs 4

instance Show User where
  show (User id name age height) =
    "User id: " ++ show id ++ " name: " ++ name ++ " age: " ++ show age ++ " height: " ++ show height



withConn :: (MySQL.Connection -> IO ()) -> IO ()
withConn query = do
  conn <- MySQL.connect $ MySQL.defaultConnectInfo {
     MySQL.connectUser     = "wai_admin"
    ,MySQL.connectPassword = "abcd1234"
    ,MySQL.connectDatabase = "wai"
    }
  query conn


test1 conn = do
  rs <- MySQL.query_ conn "SELECT * FROM test1;"
  forM_ rs $ \(id,name) -> do
    putStrLn $ "id: " ++ show (id :: Int) ++ " name:" ++ (name :: String)


test2 conn = do
  rs <- MySQL.query_ conn "SELECT * FROM test1;"
  forM_ rs $ \(IdName id name) -> do
    putStrLn $ "id: " ++ show id ++ " name:" ++ name


test3 conn = do
  rs <- MySQL.query_ conn "SELECT * FROM test2;"
  forM_ rs $ \(User id name age h) -> do
    putStrLn $
      "id: " ++ show id ++
      " name:" ++ name ++
      " age:" ++ show age ++
      " height:" ++ show h

test4 conn = do
  rs <- MySQL.query_ conn "SELECT * FROM test2;"
  forM_ rs $ \u -> do
    putStrLn $ show (u :: User)

