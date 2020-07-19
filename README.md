


# A WebApp for Santa :3

This repo is an attempt of mine to learn more about building a REST API in haskell and also to help Santa deliver presents on Christmas (_In more brash words.. This is my bribe to Santa :wink:_)

# Packages/Frameworks

1. [Spock - A web framework](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwjbkM_Y5NnqAhXnxDgGHRRQAwAQFjAAegQIAhAB&url=https%3A%2F%2Fhackage.haskell.org%2Fpackage%2FSpock&usg=AOvVaw0CgaTjXNKWCy4HpeCBbDa-)
2. [PostgreSQL-Simple](https://hackage.haskell.org/package/postgresql-simple) 

# Overview

The main code is present at `app/Main.hs`. The REST routes are provided using the Spock framework. And the data is fetched from the Postgres Database.

# Pre-requisite


## Postgres

**Install [postgres](https://www.digitalocean.com/community/tutorials/how-to-install-postgresql-on-ubuntu-20-04-quickstart)**

**Connect to  postgres**
```bash
$ sudo -i -u postgres
```

**Restore the Database** _You can create your own if you hate my entires :P_
```bash
postgres@santa:~$ createdb -T template0 restored_database
postgres@santa:~$ psql christmas < christmas_db.bak
postgres@santa:~$ psql
```

**Connect to christmas DB**
```bash
postgres=# \c christmas
```
### Databse Schema
------------------------------------

**Present**
```sql
Column |         Type           | Nullable |                     
--------+-----------------------+-----------+
 id     | integer               | not null | 
 name   | character varying(50) | not null | 
 info   | text                  | not null | 

Indexes:
     PRIMARY KEY -> (id)
```

**Location**
```sql
  Column   |     Type      | Nullable |                          
-----------+---------------+-----------+----------+
 id        | integer       | not null | 
 latitude  | numeric(10,5) | not null | 
 longitude | numeric(10,5) | not null | 

Indexes:
     PRIMARY KEY -> (id)
```

**Child**
```sql
   Column    |         Type          | Nullable |                          
-------------+-----------------------+-----------
 id          | integer               | not null | 
 name        | character varying(50) | not null | 
 naughty     | smallint              |          | 
 location_id | integer               |          |
 present_id  | integer               |          | 
Indexes:
 PRIMARY KEY -> id
Foreign-key constraints:
  FOREIGN KEY (location_id) REFERENCES location(id)
  FOREIGN KEY (present_id) REFERENCES present(id)
```

**Child_info** _A view to flatten the child table and get the info from foreign key_
```sql
    Column    |         Type          | Collation | Nullable | Default 
--------------+-----------------------+-----------+----------+---------
 child_name   | character varying(50) |           |          | 
 naughty      | smallint              |           |          | 
 latitude     | numeric(10,5)         |           |          | 
 longitude    | numeric(10,5)         |           |          | 
 present_name | character varying(50) |           |          | 
 info         | text                  |           |          | 

```

# Usage

## Build the app

```bash
$ cd santapi
$ stack build --fast  && stack exec Database-conn-exe
```

Once the app is built, you will be able to access it at `http://localhost:8080/`

## REST API's

### Fetch All Locations

_Endpoint_: http://localhost:8080/locations
_Output_:
```json
[
  {
    "locLong": -61.7879,
    "locLat": 42.5462
  },
  {
    "locLong": 114.7253,
    "locLat": 35.234
  },
  {
    "locLong": 908.3912,
    "locLat": 45.2131
  },
 ...
 ...
]
```

### Fetch All Presents

_Endpoint_: http://localhost:8080/presents
_Output_:
```json
[
  {
    "presentName": "Bike",
    "presentInfo": "A GTX sports bike limited edition"
  },
  {
    "presentName": "PS4",
    "presentInfo": "Last of us Mega Edition"
  },
  {
    "presentName": "Toy Train",
    "presentInfo": "Cheap ass toy train, Ran out of budget"
  }
 ...
 ...
]
```

### Fetch All Children

_Endpoint_: http://localhost:8080/children
_Output_:
```json
[
 {
    "childLocation": {
      "locLong": 908.3912,
      "locLat": 45.2131
    },
    "childNaughty": 10,
    "childName": "Elend Venture",
    "childPresent": {
      "presentName": "Bike",
      "presentInfo": "A GTX sports bike limited edition"
    }
  },
  {
    "childLocation": {
      "locLong": -61.7879,
      "locLat": 42.5462
    },
    "childNaughty": 80,
    "childName": "Vin Misty",
    "childPresent": {
      "presentName": "Telescope",
      "presentInfo": "Super magnified telescope"
    }
  },
 ...
 ...
]
```

### Fetch A present with particular ID

_Endpoint_: http://localhost:8080/present/<id>

### Fetch A location with particular ID

_Endpoint_: http://localhost:8080/location/<id>

### POST a present entry into table

_Endpoint_: http://localhost:8080/addPresent/

We'll use curl for POST request
```bash
$ curl -H "Content-Type: application/json" -d '{"presentName": "Radio", "presentInf": "HamRadio set"}' localhost:8080/addPresent 
```

### POST a location entry into table

_Endpoint_: http://localhost:8080/addLocation/

We'll use curl for POST request
```bash
$ curl -H "Content-Type: application/json" -d '{"locLat": 87.32, "locLong": -16.34}' localhost:8080/addLocation
```

# TODO

- [ ] Implement a POST request for Child table (Might require changes to the Datatype)
- [ ] Implement a GET request for Child with particular id
- [ ] Write a Blog Post explaining this
