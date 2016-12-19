{-# LANGUAGE OverloadedStrings #-}

module JSON where

import Data.Aeson.Types (ToJSON,toJSON,object,(.=),FromJSON,(.:))
import Data

instance ToJSON Genre where
  toJSON (Genre id t ls) =
    object ["id" .= id
           ,"title" .= t
           ,"looks" .= ls
           ]

instance ToJSON Tag where
  toJSON (Tag id t ls) =
    object ["id" .= id
           ,"title" .= t
           ,"looks" .= ls
           ]

instance ToJSON Look where
  toJSON l =
    object ["id"          .= lookId l
           ,"create_dt"   .= lookCreateDt l
           ,"update_dt"   .= lookUpdateDt l
           ,"title"       .= lookTitle l
           ,"description" .= lookDescription l
           ,"genre"       .= lookGenre l
           ,"tags"        .= lookTags l
           ]

instance ToJSON Genre' where
  toJSON (Genre' (Genre id t _)) =
    object ["id" .= id
           ,"title" .= t
           ]

instance ToJSON Tag' where
  toJSON (Tag' (Tag id t _)) =
    object ["id" .= id
           ,"title" .= t
           ]



instance ToJSON OK where
  toJSON (OK) =
    object ["result" .= ("ok" :: String)]
    
instance ToJSON NG where
  toJSON (NG c m) =
    object ["result" .= ("ng" :: String)
           ,"code" .= c
           ,"message" .= m
           ]
