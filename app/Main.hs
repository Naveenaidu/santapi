{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics

import Data.Text (Text)
import Control.Monad
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField

-- Create the eq datatypes for the tables present
-- I don't think ID's are important to mention in the datatype,
-- We could simply do "SELECT name, info FROM present" 
data Present = Present  { presentName :: Text
                        , presentInfo :: Text
                        } deriving (Show)

data Location = Location  { locLat :: Rational
                          , locLong :: Rational
                          } deriving (Show)

data Child = Child  { childName :: Text
                    , childNaughty :: Int
                    , childLocation :: Location
                    , childPresentID :: Present 
                    } deriving (Show)

-- Instances to convert between postgresql and haskell datatypes
instance FromRow Present where
  fromRow = Present <$> field <*> field

instance FromRow Location where
  fromRow = Location <$> field <*> field

instance ToRow Location where
  toRow l = [ toField (fromRational (locLat l) :: Double )
            , toField ( fromRational (locLong l) :: Double )
            ]

-- Do the stuff at DB level itself to get the type of data you want in Haskell.
-- Don't try to convert the DB data in Haskell code.
-- For eg: The Child table in DB has two foreign key `location_id` and `present_id`. But the datatype
-- in haskell has the Location field and Present field. So make a VIEW on the DB side which will use 
-- the link the id's from child table to location and present and return the entire info.
-- I initially tried to get the id in the code and execute queries using that. But that did not work. 
instance FromRow Child where
  fromRow = Child <$> field <*> field <*> liftM2 Location field field <*> liftM2 Present field field


-- FIXME: How to display the Rational datatype as decimal places
main :: IO ()
main = do
  conn <-
    connect
      defaultConnectInfo
      { connectHost = "localhost"
      , connectDatabase = "christmas"
      , connectUser = "postgres"
      , connectPassword = "admin"
      }
  mapM_ print =<< (query_ conn "SELECT name, info FROM present" :: IO [Present])
  mapM_ print =<< (query_ conn "SELECT latitude, longitude FROM location" :: IO [Location])
  mapM_ print =<< (query_ conn "SELECT * FROM child_info" :: IO [Child])
