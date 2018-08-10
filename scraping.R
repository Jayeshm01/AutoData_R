#install.packages('Rfacebook')
library(RCurl)
library(XML)
library(stringr)
library(plyr)
library(Rfacebook)
library(httr)


u = "http://www.elections.state.md.us/elections/2012/election_data/index.html"
page_parse = htmlParse(u, encoding = "utf-8")
links = getHTMLLinks(u)
filenames = links[str_detect(links, "_General.csv")]
filenames_list = as.list(filenames)
filenames_list

downloadCSV = function(filename, baseurl, folder) {
  dir.create(folder, showWarnings = FALSE)
  fileurl = str_c(baseurl, filename)
  if (!file.exists(str_c(folder, "/", filename))) {
    download.file(fileurl,
                  destfile = str_c(folder, "/", filename))
    Sys.sleep(1)
  }
}


l_ply(filenames_list, downloadCSV,
      baseurl = "http://www.elections.state.md.us/elections/2012/election_data/",
      folder = "elec12_maryland")


length(list.files("./elec12_maryland"))
list.files("./elec12_maryland")[1:5]



url = "http://planning.maryland.gov/Redistricting/2010/legiDist.shtml"
links = getHTMLLinks(url)
filenames = links[str_detect(links, "2010maps/Leg/Districts_")]
filenames_list = str_extract_all(filenames, "Districts.+pdf")
basename(filenames)


downloadPDF = function(filename, baseurl, folder, handle) {
  dir.create(folder, showWarnings = FALSE)
  fileurl = str_c(baseurl, filename)
  if (!file.exists(str_c(folder, "/", filename))) {
    pdf_file = getBinaryURL(fileurl, curl = handle)
    writeBin(pdf_file, str_c(folder, "/", filename))
    Sys.sleep(1)
  }
}

handle = getCurlHandle()
handle = getCurlHandle(useragent = str_c(R.version$platform, R.version$version.string, sep=", "), httpheader = c(from = "eddie@datacollection.com"))
l_ply(filenames_list, downloadPDF,
      baseurl = "planning.maryland.gov/PDF/Redistricting/2010maps/Leg/",
      folder = "elec12_maryland_maps",
      handle = handle)

length(list.files("./elec12_maryland_maps"))
list.files("./elec12_maryland_maps")[1:5]







ftp = 'ftp://cran.r-project.org/pub/R/web/views/'

getHTMLLinks(ftp) 


ftp_files = getURL(ftp, dirlistonly = TRUE)
cat(ftp_files)

filenames = strsplit(ftp_files, "\r\n")[[1]]
filenames_html = unlist(str_extract_all(filenames, ".+(.html)"))
filenames_html

filenames_html = getURL(ftp, customrequest = "NLST *.html")
filenames_html = strsplit(filenames_html, "\\\r\\\n")[[1]]
filenames_list = as.list(filenames_html)

downloadFTP = function(filename, folder, handle) {
  dir.create(folder, showWarnings = FALSE)
  fileurl = str_c(ftp, filename)
  if (!file.exists(str_c(folder, "/", filename))) {
    datafile = try(getURL(fileurl, curl = handle))
    write(datafile, str_c(folder, "/", filename))
    Sys.sleep(1)
  }
}
handle = getCurlHandle(ftp.use.epsv = FALSE)
l_ply(filenames_html, downloadFTP, folder = "cran_tasks", handle = handle)

length(list.files("./cran_tasks"))
list.files("./cran_tasks")[1:5]



getPageURLs = function(url) {
  baseurl = htmlParse(url)
  total_pages = as.numeric(xpathSApply(baseurl, "//div[@id='Page']/strong[2]", xmlValue))
  max_url = (total_pages - 1)*10
  add_url = str_c("/P", seq(10, max_url, 10))
  urls_list = as.list(str_c(url, add_url))
  urls_list[length(urls_list) + 1] = url
  return(urls_list)
}

url = "http://www.transparency.org/news/pressreleases/year/2010"
urls_list = getPageURLs(url)

dlPages = function(pageurl, folder ,handle) {
  dir.create(folder, showWarnings = FALSE)
  page_name = str_c(str_extract(pageurl, "/P.+"), ".html")
  if (page_name == "NA.html") { page_name = "/base.html" }
  if (!file.exists(str_c(folder, "/", page_name))) {
    content = try(getURL(pageurl, curl = handle))
    write(content, str_c(folder, "/", page_name))
    Sys.sleep(1)
  }
}

handle = getCurlHandle()
l_ply(urls_list, dlPages, folder = "tp_index_2010", handle = handle)
list.files("tp_index_2010")[1:3]
getPressURLs = function(folder) {
  pages_parsed = lapply(str_c(folder, "/", dir(folder)), htmlParse)
  urls = unlist(llply(pages_parsed, getHTMLLinks))
  press_urls = urls[str_detect(urls, "http.+/pressrelease/")]
  press_urls_list = as.list(press_urls)
  return(press_urls_list)
}
press_urls_list = getPressURLs(folder = "tp_index_2010")

dlPress = function(press_url, folder, handle) {
  dir.create(folder, showWarnings = FALSE)
  press_filename = str_c(str_extract(press_url, "[^//][[:alnum:]_.]+$") , ".html")
  if (!file.exists(str_c(folder, "/", press_filename))) {
    content = try(getURL(press_url, curl = handle))
    write(content, str_c(folder, "/", press_filename))
    Sys.sleep(1)
  }
}
handle = getCurlHandle()
l_ply(press_urls_list, dlPress, folder = "tp_press_2010", handle = handle)



mac_url    = "http://en.wikipedia.org/wiki/Machiavelli"
mac_source = readLines(mac_url, encoding = "UTF-8")
mac_parsed = htmlParse(mac_source, encoding = "UTF-8")
mac_node   = mac_parsed["//p"][[1]]

getHTMLLinks(mac_url)[1:3]
getHTMLLinks(mac_source)[1:3]
getHTMLLinks(mac_parsed)[1:3]
getHTMLLinks(mac_node)[1:3]

getHTMLLinks(mac_source, xpQuery = "//a[@class='extiw']/@href")[1:3]

xpath = "//img[contains(@src, 'Machiavelli')]/@src"
getHTMLExternalFiles(mac_source, xpQuery = xpath)[1:3]

readHTMLList(mac_source)
readHTMLList(mac_source)[[10]][1:3]

names(readHTMLTable(mac_source))
readHTMLTable(mac_source)$persondata
readHTMLTable(mac_source, stringsAsFactors = F)[[1]][7:8,1]

influential = readHTMLTable(mac_source,
                             elFun = getHTMLLinks,              stringsAsFactors = FALSE)[[1]][7,]
as.character(influential)[1:3]

influenced = readHTMLTable(mac_source,
                            elFun = getHTMLLinks, stringsAsFactors = FALSE)[[1]][8,]
as.character(influenced)[1:3]


query = "trust"
wordnetFun = function(query) {
  url = sprintf("http://wordnetweb.princeton.edu/perl/webwn?s=%s&sub=Search+WordNet", query)
  getURL(url)
}

info   = debugGatherer()
handle = getCurlHandle(cookiejar      = "", 
                        followlocation = TRUE, 
                        autoreferer    = TRUE,
                        debugfunc      = info$update,
                        verbose        = TRUE,
                        httpheader     = list(
                          from         = "Eddie@r-datacollection.com",
                          'user-agent' = str_c(R.version$version.string, 
                                               ", ", R.version$platform)
                        ))

xmlAttrsToDF = function(parsedHTML, xpath){
  x = xpathApply(parsedHTML, xpath, xmlAttrs)
  x = lapply(x, function(x) as.data.frame(t(x)))
  do.call(rbind.fill, x)
}

url        = "http://wordnetweb.princeton.edu/perl/webwn"
html_form   = getURL(url, curl = handle)
parsed_form = htmlParse(html_form)
xmlAttrsToDF(parsed_form, "//form")
xpathSApply(parsed_form, "//form")

xmlAttrsToDF(parsed_form, "//form[1]/input")

html_form_res   = getForm(uri=url, curl=handle, s="data")
parsed_form_res = htmlParse(html_form_res)
xpathApply(parsed_form_res,"//li", xmlValue)

cat(str_split(info$value()["headerOut"], "\r")[[1]])
info$reset()


url  = "http://read-able.com/"
form = htmlParse(getURL(url = url, curl = handle))
xmlAttrsToDF(form, "//form")  

xmlAttrsToDF(form, "//form[2]//input")
xpathApply(form, "//form[2]")

sentence = '"It is a capital mistake to theorize before one has data. Insensibly one begins to twist facts to suit theories, instead of theories to suit facts." - Arthur Conan Doyle, Sherlock Holmes'

res = postForm(uri=str_c(url, "check.php"), 
                curl=handle,
                style="POST",
                directInput=sentence)

readHTMLTable(res)

cat(str_split(info$value()["headerOut"],"\r")[[1]])
cat(str_split(info$value()["dataOut"],"\r")[[1]])
info$reset()


url = "r-datacollection.com/materials/http/sky.png"
sky = getBinaryURL(url=url)
writeBin(sky, "sky.png")

handle = getCurlHandle()
url   = "http://www.fixpicture.org/"
form  = htmlParse(getURL(url = url, curl = handle))

xmlAttrsToDF(form, "//form") 
xmlAttrsToDF(form, "//input")[1:2 ,c("name","type","class","value")] 
xmlAttrsToDF(form, "//select")
xmlAttrsToDF(form, "//select/option")

res = postForm(uri = "http://www.fixpicture.org/resize.php?LANG=en", 
                image = fileUpload(filename = "sky.png", 
                                   contentType = "image/png"),
                format = "pdf",
                curl = handle)

doc  = htmlParse(res)
link = str_c(url, xpathApply(doc,"//a/@href", as.character)[[1]])

resImage = getBinaryURL(link, curl=handle)
writeBin(resImage,"sky.pdf",useBytes=T)


install.packages("RHTMLForms", 
                 repos ="http://www.omegahat.net/R", 
                 type  ="source")
library(RHTMLForms)

url          = "http://wordnetweb.princeton.edu/perl/webwn"
forms        = getHTMLFormDescription(url)
formFunction = createFunction(forms[[1]])

html_form_res   = formFunction(s = "data", .curl = handle)
parsed_form_res = htmlParse(html_form_res)
xpathApply(parsed_form_res,"//li", xmlValue)

cat(str_split(info$value()["headerOut"],"\r")[[1]])




url = "www.r-datacollection.com/materials/solutions"
cat(getURL(url, userpwd = "teacher:sesame", followlocation = TRUE))

options(RDataCollectionLogin = "teacher:sesame")
cat(getURL(url, userpwd = getOption("RDataCollectionLogin"), followlocation = TRUE))


u = "https://www.icpsr.umich.edu/icpsrweb/ICPSR/ssvd/search"
u_action = "https://www.icpsr.umich.edu/icpsrweb/ICPSR/ssvd/variables?"

u = "https://www.icpsr.umich.edu/icpsrweb/ICPSR/ssvd/search"
getURL(u)

signatures = system.file("CurlSSL", "cacert.pem", package = "RCurl")
res = getURL(u, cainfo = signatures)

res = getURL(u, ssl.verifypeer = FALSE)

u_action = "https://www.icpsr.umich.edu/icpsrweb/ICPSR/ssvd/variables?"
handle = getCurlHandle(cainfo = signatures)
res = getForm(u_action, variableLabel="climate+change", questionText="", categoryLabel="", curl = handle)
str_extract(res, "Your query returned [[:digit:]]+ variables")


info   = debugGatherer()
handle = getCurlHandle(cookiejar      = "", 
                        followlocation = TRUE, 
                        autoreferer    = TRUE,
                        debugfunc      = info$update,
                        verbose        = TRUE,
                        httpheader     = list(
                          from         = "eddie@r-datacollection.com",
                          'user-agent' = str_c(R.version$version.string, 
                                               ", ", R.version$platform)
                        ))


search_url = "www.biblio.com/search.php?keyisbn=data"
cart_url   = "www.biblio.com/cart.php"

search_page = htmlParse(getURL(url = search_url, curl = handle))

xpathApply(search_page, "//div[@class='order-box'][position()<2]/form")

xpath = "//div[@class='order-box'][position()<4]/form/input[@name='bid']/@value"
bids = unlist(xpathApply(search_page, xpath, as.numeric))
bids

for(i in seq_along(bids))  {
  res = getForm(uri = cart_url, 
                 curl = handle, 
                 bid = bids[i], 
                 add = 1, 
                 int = "keyword_search")
}

cart  = htmlParse(getURL(url=cart_url, curl=handle))
clean = function(x)  str_replace_all(xmlValue(x),"(\t)|(\n\n)","")
cat(xpathSApply(cart, "//div[@class='title-block']", clean))

cat(str_split(info$value()["headerOut"],"\r")[[1]][1:13])

cat(str_split(info$value()["headerIn"],"\r")[[1]][1:14])



feed_url = "http://weather.yahooapis.com/forecastrss"
feed = getForm(feed_url , .params = list(w = "2422673", u = "c"))
parsed_feed = xmlParse(feed)

xpath = "//yweather:location|//yweather:wind|//yweather:condition"
conditions = unlist(xpathSApply(parsed_feed, xpath, xmlAttrs))

location = t(xpathSApply(parsed_feed, "//yweather:location", xmlAttrs))
forecasts = t(xpathSApply(parsed_feed, "//yweather:forecast", xmlAttrs))
forecast = merge(location, forecasts)


options(yahooid = "v.m4rTvV34GgKVVL5PEAG1uIcHyKfmY8mCJjqSl7Gx3Jkp3s2B14xCc89rQYKOmN8nc.OFbL")

baseurl = "http://where.yahooapis.com/v1/places.q('%s')"
woeid_url = sprintf(baseurl, URLencode("Hoboken, NJ, USA")) # careful: URL encoding!
parsed_woeid = xmlParse((getForm(woeid_url, appid = getOption("yahooid"))))
woeid = xpathSApply(parsed_woeid, "//*[local-name()='locality1']", xmlAttrs)[2,] # careful: namespaces!


getWeather = function(place = "New York", ask = "current", temp = "c") {
  if (!ask %in% c("current","forecast")) {
    stop("Wrong ask parameter. Choose either 'current' or 'forecast'.")
  }
  if (!temp %in%  c("c", "f")) {
    stop("Wrong temp parameter. Choose either 'c' for Celsius or 'f' for Fahrenheit.")
  }	
  base_url = "http://where.yahooapis.com/v1/places.q('%s')"
  woeid_url = sprintf(base_url, URLencode(place))
  parsed_woeid = xmlParse((getForm(woeid_url, appid = getOption("yahooid"))))
  woeid = xpathSApply(parsed_woeid, "//*[local-name()='locality1']", xmlAttrs)[2,]
  feed_url = "http://weather.yahooapis.com/forecastrss"
  parsed_feed = xmlParse(getForm(feed_url, .params = list(w = woeid, u = temp)))
  if (ask == "current") {
    xpath = "//yweather:location|//yweather:condition"
    conds = data.frame(t(unlist(xpathSApply(parsed_feed, xpath, xmlAttrs))))
    message(sprintf("The weather in %s, %s, %s is %s. Current temperature is %s°%s.", conds$city, conds$region, conds$country, tolower(conds$text), conds$temp, toupper(temp)))
  }
  if (ask == "forecast") {
    location = data.frame(t(xpathSApply(parsed_feed, "//yweather:location", xmlAttrs)))
    forecasts = data.frame(t(xpathSApply(parsed_feed, "//yweather:forecast", xmlAttrs)))
    message(sprintf("Weather forecast for %s, %s, %s:", location$city, location$region, location$country))
    return(forecasts)
  }
}

getWeather(place = "Paris", ask = "current", temp = "c")
getWeather(place = "Moscow", ask = "current", temp = "c")
getWeather(place = "Bern", ask = "forecast", temp = "c")




library(httr)

facebook = oauth_endpoint(
  authorize = "https://www.facebook.com/dialog/oauth",
  access = "https://graph.facebook.com/oauth/access_token")

Sys.setenv(FACEBOOK_CONSUMER_SECRET = "3983746230hg8745389234...") # add consumer secret here

fb_app = oauth_app("facebook", "485980054864321")

permissions = "user_birthday, user_hometown, user_location, user_status,
user_checkins, friends_birthday, friends_hometown, friends_location,
friends_relationships, friends_status, friends_checkins, publish_actions,
read_stream, export_stream"

fb_token = oauth2.0_token(facebook, fb_app, scope = permissions, type =
                             "application/x-www-form-urlencoded")

fb_sig = sign_oauth2.0(fb_token)



getUsers("hadleywickham", fb_sig, private_info = FALSE)

friends = getFriends(fb_sig, simplify = TRUE)
nrow(friends)
table(friends_info$gender)


facebook_robotstxt = "https://www.facebook.com/robots.txt" 
source("robots-parser.r")

robotsCheck(robotstxt = facebook_robotstxt, useragent = "*", dirs = "disallowed")

robotsCheck(robotstxt = facebook_robotstxt, useragent = "Yeti", dirs = "disallowed")
url = "http://www.imdb.com/chart/top"
top = getURL(url)
parsed_top = htmlParse(top, encoding = "UTF-8")
top_table = readHTMLTable(parsed_top)[[1]]
head(top_table[, 1:3])


getURL(url, useragent = str_c(R.version$platform, R.version$version.string, sep = ", "), httpheader = c(from = "eddie@datacollection.com"))


info = debugGatherer()
httpheader = list(from = "Eddie@r-datacollection.com", 'user-agent' = str_c(R.version$version.string, ", ", R.version$platform))
handle = getCurlHandle(debugfunc = info$update, verbose = TRUE)
getBest = function(doc) readHTMLTable(doc)[[1]][, 1:3]

url = "http://www.imdb.com/chart/top"
best_doc = getURL(url)
best_vec = getBest(best_doc)
if (!file.exists("bestFilms.Rdata")) {
  save(best_vec, file = "bestFilms.Rdata")
}
head(best_vec)

httpheader$"If-Modified-Since" = "Tue, 04 Mar 2014 10:00:00 GMT" 
best_doc = getURL(url, curl = handle, httpheader = httpheader)


writeLines(str_replace_all(getURL("http://www.r-datacollection.com/materials/http/HTTPdate.r"),"\r",""),"httpdate.r") # download httpdate functions
source("httpdate.r")
(last_mod = file.date("bestFilms.Rdata"))

httpheader$"If-Modified-Since" = HTTPdate(last_mod)
best_doc = getURL(url, curl = handle, httpheader = httpheader)

getCurlInfo(handle)$response.code

if (getCurlInfo(handle)$response.code == 200) {
  best_list = getBest(best_doc)
  save(best_list, file = "bestFilms.Rdata")
}


