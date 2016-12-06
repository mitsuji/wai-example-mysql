
module Data (GenreId
            ,TagId
            ,LookId
            ,Title
            ,Description
            ,CreateDT
            ,UpdateDT
            ,Genre(..)
            ,Tag(..)
            ,Look(..)
            ,Genre'(..)
            ,Tag'(..)
            ,OK(..)
            ,NG(..)
            ) where

import Data.Time.Clock (UTCTime)

type GenreId = Int
type TagId = Int
type LookId = Int
type Title = String
type Description = String
type CreateDT = UTCTime
type UpdateDT = UTCTime


data Genre = Genre GenreId Title [Look]
           deriving (Eq)

data Tag = Tag TagId Title [Look]
         deriving (Eq)

data Look = Look { lookId :: LookId
                 , lookCreateDt :: CreateDT
                 , lookUpdateDt :: UpdateDT
                 , lookTitle :: Title
                 , lookDescription :: Description
                 , lookGenre :: Genre'
                 , lookTags :: [Tag']
                 }
          deriving (Eq)


newtype Genre' = Genre' Genre
               deriving (Eq)
                        
newtype Tag' = Tag' Tag
               deriving (Eq)


data OK = OK

data NG = NG Int String

