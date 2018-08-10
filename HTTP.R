#install.packages('RCurl')
#install.packages('httr')
library(RCurl)
library(httr)
library(stringr)

packageVersion("RCurl")
ls("package:RCurl")
lsf.str("package:RCurl")

t <- "I'm Eddie! How are you & you? 1 + 1 = 2"
(url <- URLencode(t, reserve = TRUE))
URLdecode(url)

R.version$version.string
R.version$platform
cat(getURL("http://httpbin.org/headers",
           useragent = str_c(R.version$platform,
                             R.version$version.string,
                             sep=", ")))

cat(getURL("http://httpbin.org/headers", referer = "http://www.r-datacollection.com/"))

cat(getURL("http://httpbin.org/headers", httpheader = c(From = "eddie@r-datacollection.com")))

cat(getURL("http://httpbin.org/headers", cookie = "id=12345;domain=httpbin.org"))

(secret <- base64("This is a secret message"))
base64Decode(secret)

cat(getURL("http://httpbin.org/headers", proxy = "109.205.54.112:8080", followlocation = TRUE)) # this is a fictional proxy IP address and will not work


curlVersion()$protocols


names(getCurlOptionsConstants())


cat(getURL("http://www.r-datacollection.com/materials/http/helloworld.html"))

pngfile <- getBinaryURL("http://www.r-datacollection.com/materials/http/sky.png")
writeBin(pngfile, "sky.png")

url <- "http://www.r-datacollection.com/materials/http/GETexample.php"
namepar <- "Eddie"
agepar <- "32"
url_get <- str_c(url, "?", "name=", namepar, "&", "age=", agepar)
cat(getURL(url_get))

url <- "http://www.r-datacollection.com/materials/http/POSTexample.php"
cat(postForm(url, name = "Eddie", age = 32, style="post")) 

url <- "r-datacollection.com/materials/http/helloworld.html"
res <- getURL(url = url, customrequest = "HEAD", header = TRUE)
cat(str_split(res,"\r")[[1]])

url <- "www.r-datacollection.com/materials/http/helloworld.html"
(pres <- curlPerform(url = url))
pres <- NULL

performOptions <- curlOptions(url = url, writefunc = function(con) pres <<- con)
curlPerform(.opts = performOptions)
pres

content <- basicTextGatherer()
header <- basicTextGatherer()
debug <- debugGatherer()
performOptions <- curlOptions(url = url, writefunc = content$update,
                              headerfunc = header$update, debugfunc = debug$update, verbose = T)
curlPerform(.opts=performOptions)
str_sub(content$value(), 1, 100)
header$value()
names(debug$value())
debug$value()["headerOut"]

handle <- getCurlHandle()
handle <- getCurlHandle(useragent = str_c(R.version$platform,
                                          R.version$version.string,
                                          sep=", "),
                        httpheader = c(from = "ed@datacollection.com"),
                        followlocation = TRUE,
                        cookiefile = "")

urls <- sprintf("http://www.r-datacollection.com/materials/http/abunchofasciis/file00%d.html", 330:340)
lapply(urls, getURL, curl = handle)


url <- "www.r-datacollection.com/materials/http/helloworld.html"
res <- getURL(url = url, header = TRUE)
cat(str_split(res, "\r")[[1]])

handle <- getCurlHandle(customrequest = "HEAD")
res <- getURL(url = url, curl = handle, header = TRUE)
cat(str_split(res, "\r")[[1]])

handle2 <- dupCurlHandle(handle,
                         httpheader = c(from = "ed@datacollection.com"))

curl_options <- curlOptions(header = TRUE, customrequest = "HEAD")
res <- getURL(url = url, .opts = curl_options)

cat(postForm(url, .params = c(name = "Eddie", age = "32"),
             style = "post",
             .opts = list(useragent = "Eddie's R scraper",
                          referer = "www.r-datacollection.com")))

options(RCurlOptions = list(header = TRUE, customrequest = "HEAD"))
res <- getURL(url = url)
options(RCurlOptions = list())							 

res <- getURL("www.r-datacollection.com/materials/http/POSTexample.php",
              customrequest = "POST",
              postfields = "name=Eddie&age=32")
cat(str_split(res, "\r")[[1]])

url <- "r-datacollection.com/materials/http/ReturnHTTP.php"
res <- getURL(url = url)
cat(str_split(res, "\r")[[1]])

standardHeader <- list(
  from        = "eddie@r-datacollection.com",
  'user-agent' = str_c(R.version$platform,
                       R.version$version.string,
                       sep=", "))
res <- getURL(url = url, httpheader = standardHeader)
cat(str_split(res, "\r")[[1]])

defaultOptions <-  curlOptions(
  httpheader = list(
    from = "Eddie@r-datacollection.com",
    'user-agent'   = str_c(R.version$platform,
                           R.version$version.string,
                           sep=", ")),
  followlocation = TRUE,
  maxredirs      = 10,
  connecttimeout = 10,
  timeout        = 300,
  cookiefile     = "RCurlCookies.txt",
  cainfo = system.file("CurlSSL","cacert.pem", package = "RCurl"))
options(RCurlOptions = defaultOptions)

options(RCurlOptions = list())


getURL("http://www.stata-datacollection.com")

url <- "httpbin.org/get"
res <- getURL(url = url)
cat(res)

debugInfo <- debugGatherer()
names(debugInfo)
class(debugInfo[[1]])

url <- "r-datacollection.com/materials/http/helloworld.html"
res <- getURL(url = url,
              debugfunction = debugInfo$update,
              verbose = T)

names(debugInfo$value())				 
cat(debugInfo$value()["text"])
cat(str_split(debugInfo$value()["headerIn"], "\r")[[1]])
cat(str_split(debugInfo$value()["headerOut"], "\r")[[1]])
cat(str_split(debugInfo$value()["dataIn"], "\r")[[1]])
cat(str_split(debugInfo$value()["dataOut"], "\r")[[1]])
cat(str_split(debugInfo$value()["sslDataIn"], "\r")[[1]])
cat(str_split(debugInfo$value()["sslDataOut"], "\r")[[1]])

handle <- getCurlHandle()
url <- "r-datacollection.com/materials/http/helloworld.html"
res <- getURL(url = url, curl = handle)
handleInfo <- getCurlInfo(handle)

names(handleInfo)
handleInfo[c("total.time", "pretransfer.time")]
preTransTimeNoReuse   <- rep(NA, 10)
preTransTimeReuse <- rep(NA, 10)
url <- "r-datacollection.com/materials/http/helloworld.html"

for(i in 1:10){
  handle <- getCurlHandle()
  res <- getURL(url=url, curl=handle)
  handleInfo             <- getCurlInfo(handle)
  preTransTimeNoReuse[i] <- handleInfo$pretransfer.time
}

handle <- getCurlHandle()
for(i in 1:10){
  res <- getURL(url=url, curl=handle)
  handleInfo           <- getCurlInfo(handle)
  preTransTimeReuse[i] <- handleInfo$pretransfer.time
}

preTransTimeNoReuse
preTransTimeReuse


getCurlErrorClassNames()[c(2:4,7,8,10,23,29,35,64)]

url1 <- "wwww.r-datacollection.com/materials/http/helloworld.html"
res <- getURL(url1)

url2 <-  "www.r-datacollection.com/materials/http/helloworld.html"
res <- tryCatch(
  getURL(url = url1),
  COULDNT_RESOLVE_HOST = function(error){
    getURL(url = url2)
  },
  error = function(error){
    print(error$message)
    NA
  }
)
cat(str_split(res,"\r")[[1]])
