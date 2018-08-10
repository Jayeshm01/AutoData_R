#install.packages("RgoogleMaps")

require(stringr)
require(RCurl)
library(RgoogleMaps)

ftp  = "ftp://ftp.wcc.nrcs.usda.gov/data/climate/table/temperature/history/california/"

if(!file.exists("Data")) dir.create("Data")

filelist = getURL(ftp, dirlistonly = TRUE )
filelist = unlist(str_split(filelist,"\r\n"))
filelist = filelist[!filelist==""]
filelist

filesavg = str_detect(filelist,"tavg")
filesavg = filelist[filesavg]

urlsavg = str_c(ftp,filesavg)
urlsavg

for(i in 1:length(urlsavg)){
  print( c(i,"/",length(urlsavg)) )
  fname = paste0("Data/",filesavg[i])
  if( !file.exists(fname) ){
    download.file(urlsavg[i], fname)
    Sys.sleep( rpois(1,1) + runif(1,0,1) )
  }
}


txt = character()
for(i in 1:length(filesavg)){
  txt = c(txt, readLines(str_c("Data/",filesavg[i])))
}

txt = paste(txt,collapse="\n")

txtparts = unlist(str_split(txt, "----------\n"))

txtparts = str_replace(txtparts,"\n\\*\\*\\*This data is provisional and subject to change.","")
txtparts = str_replace(txtparts,"^\n","")
txtparts = txtparts[txtparts!=""]
year = str_extract(txtparts,"[[:digit:]]{2}  Average Air Temperature")
year = str_extract(year,"[[:digit:]]{2}")
year = ifelse(year < 20, str_c(20,year), str_c(19,year))
year = as.numeric(year)


station = str_extract(txtparts, "Station : .+?\n")
station     = str_replace_all(station, "(Station : )|(\n)", "")
station     = str_split(station,", ")
id          = sapply(station, '[', 1)
name        = sapply(station, '[', 2)



start = proc.time()
temperatures = str_extract(txtparts, "day[\\s\\S]*") 
tempData = data.frame(avgtemp=NA, day=NA, month=NA, year=NA, id="", name="")
day      = rep(1:31, 12)
month    = rep( c(10:12,1:9), each=31 ) 

if(F==T){
  for(i in seq_along(txtparts)){
    tf = tempfile()
    writeLines(temperatures[i], tf)
    temptable = read.fwf(tf, width=c(3,7,6,6,6,6,6,6,6,6,6,6,6), stringsAsFactors=F)
    temptable = temptable[3:33, -1]
    temptable = suppressWarnings(as.numeric(unlist(temptable)))
    temptable = data.frame( avgtemp=temptable, day=day,      month=month, 
                             year=year[i],      name=name[i], id=id[i]     )
    tempData = rbind(tempData, temptable)
  }
  proc.time() - start
}

parseTemp = function(filename){
  txt = paste( readLines(filename), collapse="\n")
  txtparts = unlist(str_split(txt, "----------\n"))
  txtparts = str_replace(txtparts,
                          "\n\\*\\*\\*This data is provisional and subject to change.","")
  txtparts = str_replace(txtparts,"^\n","")
  txtparts = txtparts[txtparts!=""]
  year = str_extract(txtparts,"[[:digit:]]{2}  Average Air Temperature")
  year = str_extract(year,"[[:digit:]]{2}")
  year = ifelse(year < 20, str_c(20,year), str_c(19,year))
  year = as.numeric(year)
  station = str_extract(txtparts, "Station : .+?\n")
  station     = str_replace_all(station, "(Station : )|(\n)", "")
  station     = str_split(station,", ")
  id          = sapply(station, '[', 1)
  name        = sapply(station, '[', 2)
  temperatures = str_extract(txtparts, "day[\\s\\S]*") 
  tempData = data.frame(avgtemp=NA, day=NA, month=NA, year=NA, id="", name="")
  day      = rep(1:31, 12)
  month    = rep( c(10:12,1:9), each=31 ) 
  doTemp = function(temperatures, year, name, id){
    tf = tempfile()
    writeLines(temperatures, tf)
    temptable = read.fwf(tf, width=c(3,7,6,6,6,6,6,6,6,6,6,6,6), 
                          stringsAsFactors=F)
    temptable = temptable[3:33, -1]
    temptable = suppressWarnings(as.numeric(unlist(temptable)))
    temptable = data.frame( avgtemp=temptable, day=day, month=month, 
                             year=year, name=name, id=id )
    tempData <= rbind(tempData, temptable)
  }
  mapply(doTemp, temperatures, year, name, id)
  tempData = tempData[!is.na(tempData$avgtemp),]
  return(tempData)
}

parseTemps = function(filenames){
  tmp = lapply(filenames, parseTemp)
  tempData = NULL
  for(i in seq_along(tmp)) tempData = rbind(tempData,tmp[[i]])
  return(tempData)
}

tempData = parseTemps( str_c("Data/",filesavg) )
download.file("ftp://ftp.wcc.nrcs.usda.gov/states/ca/jchen/CA_sites.dat",
              "Data/CA_sites.dat")

stationData = read.csv("data/CA_sites.dat", header=F, sep="|")[,-c(1,2,7:9)]
names(stationData) = c("name","lat","lon","alt")

stationData$lon = stationData$lon * -1
stationData[,c("lat","lon")] = stationData[,c("lat","lon")]/100
stationData$alt = stationData$alt / 3.2808399 
stationData = stationData[order(stationData$lat),]


center  = c(mean(range(stationData$lat)),mean(range(stationData$lon)))
map1 = GetMap(
  destfile = "map1.png",  
  zoom=7,
  size=c(640,500),
  GRAYSCALE=T,
  center=center,
  maptype="hybrid",
  NEWMAP=FALSE)

map2 = GetMap.OSM(
  latR=c(37.5,42),
  lonR=c(-125,-115),
  scale=5000000,
  destfile = "map2.png",
  GRAYSCALE=TRUE,
  NEWMAP=FALSE)
tmp = readPNG("map2.png", native = FALSE)
tmp = RGB2GRAY(tmp)
writePNG(tmp, "map2.png")


png("stationmap1.png",
    width=dim(readPNG("map1.png"))[2],
    height=dim(readPNG("map1.png"))[1])
PlotOnStaticMap(map1, 
                lat = stationData$lat, 
                lon = stationData$lon, 
                cex=2, pch=19,
                col=rgb(1,1,1,0.8), add=FALSE)
dev.off()

png("stationmap2.png",
    width=dim(readPNG("map2.png"))[2],
    height=dim(readPNG("map2.png"))[1])
PlotOnStaticMap(map2, 
                lat = stationData$lat, 
                lon = stationData$lon, 
                cex=2, pch=19,
                col=rgb(0,0,0,0.5), add=FALSE)
dev.off()

monthlyTemp = aggregate(tempData$avgtemp, by=list(name=tempData$name,month=tempData$month), mean)
stationNames = c(  "ADIN MTN", "INDEPENDENCE CAMP", "SQUAW VALLEY G.C.",
                    "SPRATT CREEK", "LEAVITT MEADOWS","POISON FLAT")
stationAlt   = stationData[match(stationNames, stationData$name),]$alt
stationLat   = stationData[match(stationNames, stationData$name),]$lat
stationLon   = stationData[match(stationNames, stationData$name),]$lon

plotTemps = function(i){
  iffer = monthlyTemp$name==stationNames[i]
  plot( monthlyTemp[iffer, c("month","x")],
        type="b", 
        main=str_c(stationNames[i],
                   " (",round(stationAlt[i]),
                   "m)", 
                   "\n Lat.= ", stationLat[i],
                   " Lon.= ",   stationLon[i]),
        ylim=c(-15,25),
        ylab="average temperature" )
  abline(h=0,lty=2)
  iffer2 = tempData$name==stationNames[i]
  points( tempData$month[iffer2] + tempData$day[iffer2] *0.032, 
          jitter(tempData$avgtemp[iffer2],3), col=rgb(0.2,0.2,0.2,0.1),pch=".")
}

pdf("stationstemp.pdf", width=9, height=6)
par(mfrow=c(2,3)) 
for(i in seq_along(stationNames)) plotTemps(i)
par(mfrow=c(1,1)) 
dev.off()

