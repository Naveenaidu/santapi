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
import Database.PostgreSQL.Simple.SqlQQ (sql)


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

instance ToRow Present where
  toRow p = [toField (presentName p), toField (presentInfo p)]

instance FromRow Location where
  fromRow = Location <$> field <*> field

instance ToRow Location where
  toRow l = [ toField (fromRational (locLat l) :: Double )
            , toField ( fromRational (locLong l) :: Double )
            ]

instance FromRow Child where
  fromRow = Child <$> field <*> field 
            <*> liftM2 Location field field 
            <*> liftM2 Present field field

instance Show Location where
  show (Location lat long) = 
    "Location {locLat= (" ++  show (fromRational lat :: Double) ++ ") , locLong =(" 
    ++ show (fromRational long :: Double) ++ ")}"

-- Instances to convert between haskell datatype and JSON object
instance ToJSON Location where
  toJSON (Location lat long) = object ["locLat" .= (fromRational lat :: Double),
                                       "locLong" .= (fromRational long :: Double) ]
instance FromJSON Location

instance ToJSON Present
instance FromJSON Present

instance ToJSON Child
instance FromJSON Child
---------------------------------------------------------------------------------------------
-- DB querying

getLocation ::(MonadReader Connection m, MonadIO m) => Int -> m [Location]
getLocation id = do
  conn <- ask 
  location <- liftIO (query conn "SELECT latitude, longitude FROM location WHERE id = ?" (Only (id :: Int)) :: IO [Location])
  return (location)

getPresent :: (MonadReader Connection m, MonadIO m) => Int -> m [Present]
getPresent id = do
  conn <- ask
  present <- liftIO (query conn "SELECT name, info FROM present WHERE id = ?" (Only (id :: Int)) :: IO [Present])
  return present

getChild :: (MonadReader Connection m, MonadIO m) => Int -> m [Child]
getChild id = do
  conn <- ask
  child <- liftIO (query conn "SELECT * FROM child_info WHERE id = ?" (Only (id :: Int)) :: IO [Child])
  return child

fetchAllLocations :: (MonadReader Connection m, MonadIO m) => m [Location]
fetchAllLocations = do
  conn <- ask
  locations <- liftIO (query_ conn "SELECT latitude,longitude FROM location ORDER BY id")
  return locations

fetchAllPresents :: (MonadReader Connection m, MonadIO m) => m [Present]
fetchAllPresents = do
  conn <- ask
  presents <- liftIO (query_ conn "SELECT name, info FROM present ORDER BY id" :: IO [Present])
  return presents

fetchAllChildren :: (MonadReader Connection m, MonadIO m) => m [Child]
fetchAllChildren = do
  conn <- ask
  children <- liftIO (query_ conn "SELECT * FROM child_info")
  return children

-----------------------------------------------------------------------------------------------------
-- Server Implementation --


-- Error Message
errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
    object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]

-- SpockM conn0 sess0 st0 ()
-- conn = DB connection type
-- sess = session type
-- st = state type
-- the monadic return type
type Api = SpockM Connection () () ()
type ApiAction a = SpockAction Connection () () a


app :: Api
app = do

  -- GET ROUTES
  get "presents" $ do
    maybePresents <- runQuery $ \conn -> runReaderT fetchAllPresents conn
    case maybePresents of
      [] -> errorJson 1 "Could not find any Present"
      otherwise -> json maybePresents
      
  get "locations" $ do
    maybeLocations <- runQuery $ (\conn -> runReaderT fetchAllLocations conn)
    case maybeLocations of
      [] -> errorJson 1 " Could not find any Locations"
      otherwise -> json $ maybeLocations

  get "children" $ do
    maybeChildren <- runQuery $ (\conn -> runReaderT fetchAllChildren conn)
    case maybeChildren of
      [] -> errorJson 1 " Could not find any Children"
      otherwise -> json $ maybeChildren

  -- Get elements with particular id
  get ("present" <//> var) $ \presentID -> do
    maybePresent <- runQuery $ \conn -> runReaderT (getPresent presentID) conn
    case maybePresent of
      [] -> errorJson 1 "Could not find any Present with matching id"
      otherwise -> json maybePresent

  get ("location" <//> var) $ \locationID -> do
    maybeLoaction <- runQuery $ \conn -> runReaderT (getLocation locationID) conn
    case maybeLoaction of
      [] -> errorJson 1 "Could not find any location with matching id"
      otherwise -> json maybeLoaction

  get ("child" <//> var) $ \childID -> do
    maybeChild <- runQuery $ \conn -> runReaderT (getChild childID) conn
    case maybeChild of
      [] -> errorJson 1 "Could not find any child with matching id"
      otherwise -> json maybeChild

  -- POST routes
  post "addPresent" $ do
    maybePresent <- jsonBody :: ApiAction (Maybe Present)
    case maybePresent of
      Nothing -> errorJson 1 "Failed to parse request body of Present"
      Just thePresent -> do
        runQuery $ \conn -> execute conn "INSERT into present (name, info) values (?,?)" (toRow thePresent)
        json $ object ["result" .= String "success"]

  post "addLocation" $ do
    maybeLocation <- jsonBody :: ApiAction (Maybe Location)
    case maybeLocation of
      Nothing -> errorJson 1 "Failed to parse request body of Location"
      Just theLocation -> do
        runQuery $ \conn -> execute conn "INSERT into location (latitude, longitude) values (?,?)" (toRow theLocation)
        json $ object ["result" .= String "success"]
  
  -- TODO: Implement a POST request for Child table

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