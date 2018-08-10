#install.packages('RSQLite')

library(RSQLite)
sqlite  = dbDriver("SQLite")
if(file.exists("birthdays.db")) {
  try( dbDisconnect(con) )
  file.remove("birthdays.db")
}
con = dbConnect(sqlite, "birthdays.db")



sql = "CREATE  TABLE  birthdays (
nameid INTEGER  NOT  NULL   ,
firstname VARCHAR(100) NOT  NULL  ,
lastname VARCHAR(100) NOT  NULL  ,
birthday DATE  ,
PRIMARY  KEY  (nameid)
) ;"
dbGetQuery(con, sql)
dbListTables(con)
dbReadTable(con,"birthdays")

sql = "CREATE  TABLE  foodtypes (
foodid INTEGER  NOT  NULL  ,
foodname VARCHAR(100) NOT  NULL,
healthy INT,
kcalp100g float,
PRIMARY  KEY  (foodid)
);"
dbGetQuery(con, sql)


sql = "CREATE  TABLE  foodranking (
rankid INT  NOT  NULL  ,
foodid INT  ,
nameid INT  ,
rank INT  NULL  ,
PRIMARY  KEY  (rankid) ,
FOREIGN  KEY  (foodid) REFERENCES  foodtypes (foodid) ON  UPDATE  CASCADE,
FOREIGN  KEY  (nameid) REFERENCES  birthdays (nameid) ON  UPDATE  CASCADE    
) ;"
dbGetQuery(con, sql)

dbListTables(con)



sql = "CREATE  TABLE  dummy (dcolumn INT) ;"
dbGetQuery(con, sql)
dbListTables(con)

sql = "DROP  TABLE  dummy ;"
dbGetQuery(con, sql)
dbListTables(con)



sql = "INSERT  INTO  birthdays (firstname, lastname, birthday)
VALUES  ('Peter', 'Pascal', '1991-02-01'),
('Paul',  'Panini', '1992-03-02'),
('Mary',  'Meyer',  '1993-04-03') ;"
dbGetQuery(con, sql)

sql = "INSERT  INTO  foodtypes (foodname, healthy,kcalp100g)
VALUES  ('spaghetti',    0, 0.158),
('hamburger',    0, 0.295),
('fruit salad',  1, 0.043),
('chocolate',    0, 0.546),
('fish fingers', 0, 0.290) ;"
dbGetQuery(con, sql)

sql = "INSERT  INTO  foodranking (rankid, nameid, foodid, rank)
VALUES  (1, 1, 1, 1),
(2, 1, 2, 2),
(3, 2, 3, 1),
(4, 3, 4, 1),
(5, 3, 5, 2),
(6, 3, 2, 3) ;"
dbGetQuery(con, sql)

sql = "ALTER  TABLE  foodtypes ADD  COLUMN  highenergy INT  ;           "
dbGetQuery(con, sql)
sql = "UPDATE  foodtypes SET  highenergy=1 WHERE  kcalp100g >  0.2 ;    "
dbGetQuery(con, sql)
sql = "UPDATE  foodtypes SET  highenergy=0 WHERE  kcalp100g <= 0.2 ;    "
dbGetQuery(con, sql)
dbReadTable(con,"foodtypes")

tmp = dbReadTable(con,"foodtypes")
dbWriteTable(con,"foodtypes",tmp,overwrite=T)
rm(tmp)

sql = "INSERT  INTO  foodtypes (foodid, foodname, healthy, kcalp100g)
VALUES  (6,'Dominic''s incredible pancakes', NULL, NULL) ;"
dbGetQuery(con, sql)
dbReadTable(con, "foodtypes",row.names="")
sql = "DELETE  FROM  foodtypes WHERE  foodname = 'Dominic''s incredible pancakes' ;"
dbGetQuery(con, sql)
dbReadTable(con, "foodtypes",row.names="")

dbGetQuery(con, "SELECT  * FROM  birthdays ;")
dbGetQuery(con, "SELECT  birthday FROM  birthdays ;")
dbGetQuery(con, "SELECT  firstname, birthday FROM  birthdays ;")

sql = "INSERT  INTO  birthdays (nameid,firstname,lastname,birthday)
VALUES  (10,'Donald','Docker','1934-06-09') ;"
dbGetQuery(con, sql)
dbGetQuery(con, "SELECT  * FROM  birthdays ;")

sql = "SELECT  birthdays.nameid, firstname, lastname, birthday, foodid, rank
FROM  birthdays
INNER  JOIN  foodranking
ON  birthdays.nameid=foodranking.nameid ;"
dbGetQuery(con, sql)

sql = "SELECT  birthdays.nameid, firstname, lastname, birthday, foodid, rank
FROM  birthdays
LEFT  JOIN  foodranking
ON  birthdays.nameid=foodranking.nameid ;"
dbGetQuery(con, sql)

sql = "SELECT  firstname, rank, foodname FROM  birthdays
INNER  JOIN  foodranking
ON  birthdays.nameid = foodranking.nameid
INNER  JOIN  foodtypes
ON  foodranking.foodid = foodtypes.foodid  ;"
dbGetQuery(con, sql)

dbGetQuery(con, "DELETE  FROM  birthdays WHERE  firstname = 'Donald' ;")


sql = "SELECT  firstname, foodname, rank FROM  birthdays
INNER  JOIN  foodranking ON  birthdays.nameid = foodranking.nameid
INNER  JOIN  foodtypes ON  foodranking.foodid = foodtypes.foodid
WHERE  rank >= 2  AND  firstname = 'Mary' ;"
dbGetQuery(con, sql)

sql = "SELECT  firstname, foodname, healthy FROM  birthdays
INNER  JOIN  foodranking ON  birthdays.nameid = foodranking.nameid
INNER  JOIN  foodtypes ON  foodranking.foodid = foodtypes.foodid
WHERE  (healthy = 1 OR  firstname < 'Peter') AND  firstname != 'Mary' ;"
dbGetQuery(con, sql)

sql = "SELECT  firstname, lastname FROM  birthdays
WHERE  firstname IN  ('Peter','Paul','Karl') ;"
dbGetQuery(con, sql)

sql = "SELECT  firstname, lastname FROM  birthdays
WHERE  firstname LIKE  '%er' OR  lastname LIKE  '%e_';"
dbGetQuery(con, sql)

sql = "SELECT  firstname FROM  birthdays ORDER  BY  firstname  ;"
dbGetQuery(con, sql)

sql = "SELECT  firstname FROM  birthdays
ORDER  BY  birthday DESC, firstname ASC  ;"
dbGetQuery(con, sql)
 
sql = "SELECT  firstname, COUNT(rank) FROM  birthdays
INNER  JOIN  foodranking ON  birthdays.nameid = foodranking.nameid
INNER  JOIN  foodtypes ON  foodranking.foodid = foodtypes.foodid
GROUP  BY    birthdays.nameid ;"
dbGetQuery(con, sql)

sql = "SELECT  firstname, COUNT(rank) FROM  birthdays
INNER  JOIN  foodranking ON  birthdays.nameid = foodranking.nameid
INNER  JOIN  foodtypes ON  foodranking.foodid = foodtypes.foodid
GROUP  BY  birthdays.nameid
HAVING  COUNT(rank) > 1 ;"
dbGetQuery(con, sql)


dbGetQuery(con, "BEGIN TRANSACTION  ;")
dbGetQuery(con, "INSERT  INTO  birthdays (firstname, lastname) VALUES  ('Simon', 'Sorcerer') ;")
dbGetQuery(con, "SELECT  firstname, lastname FROM  birthdays ;")
dbGetQuery(con, "ROLLBACK  ;")
dbGetQuery(con, "SELECT  firstname, lastname FROM  birthdays ;")


sqlite = dbDriver("SQLite")
con = dbConnect(sqlite, "birthdays.db")

sql = "SELECT * FROM birthdays"
res = dbGetQuery(con, sql)
res
res = dbSendQuery(con, sql)
fetch(res)


dbGetInfo(con)

dbListTables(con)

res = dbReadTable(con, "birthdays")
res

dbWriteTable(con, "test", res)

dbExistsTable(con, "test")

dbRemoveTable(con, "test")

dbDataType(con, res$nameid)
dbDataType(con, res$firstname)
dbDataType(con, res$birthday)

dbBeginTransaction(con)
dbRollback(con)

dbBeginTransaction(con)
dbCommit(con)

dbDisconnect(con)
