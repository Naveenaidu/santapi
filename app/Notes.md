## Postgres
-----------
1. Connect to postgres
```
$ sudo -i -u postgres
$ psql
```
2. List the Databases
```
postgres=# \l
```

3. Connect to Database
```
postgres=# \c christmas
```

4. Display all Tables
```
database_name=# \dt
```

5. See the content of a specific table
```
db_name=# \d <table_name>
```

## Thoughts
------------
-- Do the stuff at DB level itself to get the type of data you want in Haskell.
-- Don't try to convert the DB data in Haskell code.
-- For eg: The Child table in DB has two foreign key `location_id` and `present_id`. But the datatype
-- in haskell has the Location field and Present field. So make a VIEW on the DB side which will use 
-- the link the id's from child table to location and present and return the entire info.
-- I initially tried to get the id in the code and execute queries using that. But that did not work. 

-- Create the eq datatypes for the tables present
-- I don't think ID's are important to mention in the datatype,
-- We could simply do "SELECT name, info FROM present" 

-- The problem was with the type
-- query returns IO [Location], we need to convert it into ReaderT Connection IO [Location]
-- You can read ReaderT Connection IO [Location] as m a where m == ReaderT Connection IO and a == IO [Location]
-- So we can use `lift` to apply the Transformer type(ReaderT Connection) to IO [Location]

instance Show Location where
  show (Location lat long) = 
    "Location {locLat= (" ++  show (fromRational lat :: Double) ++ ") , locLong =(" 
    ++ show (fromRational long :: Double) ++ ")}"