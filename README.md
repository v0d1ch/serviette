# SERVIETTE - JSON to SQL 
  Library for generating SQL queries from JSON. 
  Send the json in the expected format and receive raw sql string.
### Why ?
- Why not ?

### Expected JSON format
````
{
    "format":1,
    "action":"SELECT",
    "selectName": "users",
    "joinTables":[
    	  {"tablename":"addresses","field":"userid","operator":"=","withTable":"users", "withField":"id"},
          {"tablename":"posts","field":"userid","operator":"=","withTable":"users", "withField":"id"}
    	],
    "whereCondition":[
          {"whereTableName":"users","whereField":"id", "whereOperator":">", "whereFieldValue": 1}
      ]
}
````

If `format` is set to 1 you will get raw sql string back:

````
SELECT users join addresses on userid = users.id join posts on userid = users.id where users.id > 1
````

### Work in progress
Error and warning messages need to be implemented as well as more complex queries. 
You can find this lib on [hackage](https://hackage.haskell.org/package/serviette)
Serviette backend is [here](https://github.com/v0d1ch/serviette-yesod-example)

