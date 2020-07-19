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
Do the stuff at DB level itself to get the type of data you want in Haskell.
Don't try to convert the DB data in Haskell code.
For eg: The Child table in DB has two foreign key `location_id` and `present_id`. But the datatype in haskell has the Location field and Present field. So make a VIEW on the DB side which will use  the link the id's from child table to location and present and return the entire info.
I initially tried to get the id in the code and execute queries using that. But that did not work. 

Create the eq datatypes for the tables present
I don't think ID's are important to mention in the datatype,
We could simply do "SELECT name, info FROM present" 

The problem was with the type
query returns IO [Location], we need to convert it into ReaderT Connection IO [Location]
You can read ReaderT Connection IO [Location] as m a where m == ReaderT Connection IO and a == IO [Location]
So we can use `lift` to apply the Transformer type(ReaderT Connection) to IO [Location]



FIXME: How will we update/insert the child value into DB
We'll ask for location id and present id, check in the code if the location/present exist
If no then display error else send the value

Maybe we might have to create a new Child' datatype for POST request\

curl -H "Content-Type: application/json" -d '{"presentName": "Radio", "presentInf": "HamRadio set"}' localhost:8080/addPresent 

curl -H "Content-Type: application/json" -d '{"locLat": 87.32, "locLong": -16.34}' localhost:8080/addLocation

## References
-------------
https://translate.google.com/translate?hl=en&sl=de&u=https://stackoverrun.com/de/q/11669883&prev=search&pto=aue

https://www.lambda-land.com/posts/2017-11-16-postgresql-simple

https://github.com/xor-xor/webapp-spock/blob/master/src/App.hs

https://chrispenner.ca/posts/monadio-considered-harmful

https://www3.ntu.edu.sg/home/ehchua/programming/sql/PostgreSQL_GetStarted.html

https://www.spock.li/tutorials/rest-api