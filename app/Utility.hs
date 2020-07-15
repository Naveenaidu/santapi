Utility functions
getLocation :: Int -> Connection -> IO [Location] 
getLocation id conn = do
  location <- (query conn "SELECT latitude, longitude FROM location WHERE id = ?" (Only (id :: Int)) :: IO [Location])
  return location

getPresent :: Int -> Connection -> IO [Present]
getPresent id conn = do
  present <- (query conn "SELECT name, info FROM present WHERE id = ?" (Only (id :: Int)) :: IO [Present])
  return present