
library(stringr)
library(RCurl)
library(XML)
library(maptools)
library(rgdal)
library(maps)
library(TeachingDemos) 
tb <- getForm("http://www.dastelefonbuch.de/", .params = c(kw = "Feuerstein", cmd = "search", ao1 = "1", reccount = "2000"))
dir.create("phonebook_feuerstein")
write(tb, file =  "phonebook_feuerstein/phonebook_feuerstein.html")


tb_parse <- htmlParse("phonebook_feuerstein/phonebook_feuerstein.html",encoding="UTF-8")

xpath <- '//ul/li/a[contains(text(), "Privat")]'
num_results <- xpathSApply(tb_parse, xpath, xmlValue)
num_results
num_results <- as.numeric(str_extract(num_results, '[[:digit:]]+'))
num_results

xpath <- '//div[@class="name"]/a[@title]'
surnames <- xpathSApply(tb_parse, xpath, xmlValue)
surnames[1:5]

xpath <- '//span[@itemprop="postal-code"]'
zipcodes <- xpathSApply(tb_parse, xpath, xmlValue)
zipcodes[1:5]

length(surnames)
length(zipcodes)


xpath <- '//span[@itemprop="postal-code"]/ancestor::div[@class="popupMenu"]/preceding-sibling::div[@class="name"]'
names_vec <- xpathSApply(tb_parse, xpath, xmlValue)
names_vec <- str_replace_all(names_vec, "(\\n|\\t|\\r| {2,})", "")
names_vec[1:5]

xpath <- '//div[@class="name"]/following-sibling::div[@class="popupMenu"]//span[@itemprop="postal-code"]'
zipcodes_vec <- xpathSApply(tb_parse, xpath, xmlValue)
zipcodes_vec <- as.numeric(zipcodes_vec)
zipcodes_vec[1:5]

entries_df <- data.frame(plz = zipcodes_vec, name = names_vec)
head(entries_df)


dir.create("geo_germany")
download.file("http://fa-technik.adfc.de/code/opengeodb/PLZ.tab", destfile="geo_germany/plz_de.txt")
plz_df <- read.delim("geo_germany/plz_de.txt", stringsAsFactors=FALSE,encoding="UTF-8")
head(plz_df)
places_geo <- merge(entries_df, plz_df, by="plz", all.x = TRUE)
head(places_geo)


download.file("http://biogeo.ucdavis.edu/data/gadm2/shp/DEU_adm.zip", destfile="geo_germany/ger_shape.zip")
unzip("geo_germany/ger_shape.zip", exdir = "geo_germany")
dir("geo_germany")

projection <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
map_germany <- readShapePoly(paste(getwd(),"/geo_germany/DEU_adm0.shp", sep=""), proj4string=projection)
map_germany_laender <- readShapePoly(paste(getwd(),"/geo_germany/DEU_adm1.shp", sep=""), proj4string=projection)


coordinates <- SpatialPoints(cbind(places_geo$lon,places_geo$lat))
proj4string(coordinates) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

data("world.cities")
cities_ger <- world.cities[world.cities$country.etc == "Germany" & (world.cities$pop > 460000 | world.cities$name =="Mannheim"  | world.cities$name =="Jena"),]
coords_cities <- SpatialPoints(cbind(cities_ger$long,cities_ger$lat))

dir.create("figures")
par(mfrow=c(1,1))
plot(map_germany)
plot(map_germany_laender, add = T)
points(coordinates$coords.x1, coordinates$coords.x2, col=rgb(10,10,10,max=255), bg=rgb(10,10,10,max=255), pch=20, cex=1)
points(coords_cities, col = "black", , bg="grey", pch=23)
shadowtext(cities_ger$long,cities_ger$lat, labels = cities_ger$name, pos = 4, col = "black", bg = "white", cex =  1.2)
