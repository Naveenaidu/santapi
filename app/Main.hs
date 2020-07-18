{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main where

import Web.Spock
import Web.Spock.Config

import Data.Aeson       hiding (json)
import Data.Monoid      ((<>))
import Data.Text        (Text, pack)
import GHC.Generics
import Data.Pool

import Data.Text (Text)
import Data.Ratio
import Control.Monad
import Control.Monad.Reader
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField


data Present = Present  { presentName :: Text
                        , presentInfo :: Text
                        } deriving (Generic, Show)

data Location = Location  { locLat :: Rational
                          , locLong :: Rational
                          } deriving (Generic)

data Child = Child  { childName :: Text
                    , childNaughty :: Int
                    , childLocation :: Location
                    , childPresent :: Present 
                    } deriving (Generic, Show)

-- Instances to convert between postgresql and haskell datatypes
instance FromRow Present where
  fromRow = Present <$> field <*> field

instance FromRow Location where
  fromRow = Location <$> field <*> field

instance ToRow Location where
  toRow l = [ toField (fromRational (locLat l) :: Double )
            , toField ( fromRational (locLong l) :: Double )
            ]


-- FIXME: How will we update/insert the child value into DB
-- We'll ask for location id and present id, check in the code if the location/present exist
-- If no then display error else send the value

-- Maybe we might have to create a new Child' datatype for POST request
instance FromRow Child where
  fromRow = Child <$> field <*> field <*> liftM2 Location field field <*> liftM2 Present field field

getLocation :: Int -> ReaderT Connection IO [Location]
getLocation id = do
  conn <- ask 
  location <- lift (query conn "SELECT latitude, longitude FROM location WHERE id = ?" (Only (id :: Int)) :: IO [Location])
  return (location)

getPresent :: Int -> ReaderT Connection IO [Present]
getPresent id = do
  conn <- ask
  present <- lift (query conn "SELECT name, info FROM present WHERE id = ?" (Only (id :: Int)) :: IO [Present])
  return present

getChild :: Int -> ReaderT Connection IO [Child]
getChild id = do
  conn <- ask
  child <- lift (query conn "SELECT * FROM child_info WHERE id = ?" (Only (id :: Int)) :: IO [Child])
  return child

-- main :: IO ()
-- main = do
--   conn <-
--     connect
--       defaultConnectInfo
--       { connectHost = "localhost"
--       , connectDatabase = "christmas"
--       , connectUser = "postgres"
--       , connectPassword = "admin"
--       }
--   mapM_ print =<< (query_ conn "SELECT name, info FROM present" :: IO [Present])
--   mapM_ print =<< (query_ conn "SELECT latitude, longitude FROM location" :: IO [Location])
--   mapM_ print =<< (query_ conn "SELECT * FROM child_info" :: IO [Child])
--   mapM_ print =<< runReaderT (getLocation 1) conn
--   execute conn "INSERT into location (latitude, longitude) values (?,?)" ((123.32 :: Double), (142.23 :: Double))
--   execute conn 
--     "INSERT into child (name,naughty,location_id,present_id) values (?,?,?,?)"
--     (("Harry" :: Text), (35 :: Int), (3 :: Int), (2 :: Int))

-----------------------------------------------------------------------------------------------------
-- Server Implementation --

-- Custom JSON String for Location
instance ToJSON Location where
  toJSON (Location lat long) = object ["locLat" .= (fromRational lat :: Double),
                                       "locLong" .= (fromRational long :: Double) ]
instance FromJSON Location

instance ToJSON Present
instance FromJSON Present

instance ToJSON Child
instance FromJSON Child

instance Show Location where
  show (Location lat long) = 
    "Location {locLat= (" ++  show (fromRational lat :: Double) ++ ") , locLong =(" 
    ++ show (fromRational long :: Double) ++ ")}"

-- SpockM conn0 sess0 st0 ()
-- conn = DB connection type
-- sess = session type
-- st = state type
-- the monadic return type
type Api = SpockM Connection () () ()
type ApiAction a = SpockAction Connection () () a

app :: Api
app = do
  get "present" $ do
    json $ Present {presentName = "Telescope", presentInfo = "Super magnified telescope"}

  get "location" $ do
    json $ Location {locLat= (42.5462) , locLong =(-61.7879)}

  get "child" $ do
    json $ Child {childName = "Koro Sensei", childNaughty = 25, childLocation = Location {locLat = (-162897) % 10000, locLong = 456721 % 10000}, childPresent = Present {presentName = "Toy Train", presentInfo = "Cheap ass toy train, Ran out of budget"}}

  post "present" $ do
    thePresent <- jsonBody' :: ApiAction Present
    text $ "Parsed: " <> pack (show thePresent)

  post "location" $ do
    theLocation <- jsonBody' :: ApiAction Location
    text $ "Parsed: " <> pack (show theLocation)

  post "child" $ do
    theChild <- jsonBody' :: ApiAction Child
    text $ "Parsed: " <> pack (show theChild)

connectDB :: IO Connection
connectDB =
  connect
      defaultConnectInfo
      { connectHost = "localhost"
      , connectDatabase = "christmas"
      , connectUser = "postgres"
      , connectPassword = "admin"
      }

main :: IO ()
main = do
    
  pool <- createPool connectDB close 1 10 10
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  runSpock 8080 (spock spockCfg app)




