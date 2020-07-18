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

instance Show Location where
  show (Location lat long) = 
    "Location {locLat= (" ++  show (fromRational lat :: Double) ++ ") , locLong =(" 
    ++ show (fromRational long :: Double) ++ ")}"
---------------------------------------------------------------------------------------------
-- DB querying

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

fetchAllLocations :: ReaderT Connection IO [Location]
fetchAllLocations = do
  conn <- ask
  locations <- lift (query_ conn "SELECT latitude,longitude FROM location ORDER BY id")
  return locations

fetchAllPresents :: ReaderT Connection IO [Present]
fetchAllPresents = do
  conn <- ask
  presents <- lift (query_ conn "SELECT name, info FROM present ORDER BY id" :: IO [Present])
  return presents

fetchAllChildren :: ReaderT Connection IO [Child]
fetchAllChildren = do
  conn <- ask
  children <- lift (query_ conn "SELECT * FROM child_info")
  return children

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




