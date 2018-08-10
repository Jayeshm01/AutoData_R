library(XML)
library(stringr)
library(plyr)
library(RCurl)

getStock = function(x){
  date = xmlValue(x[["date"]])
  value = xmlValue(x[["close"]])
  c(date, value)
}

all_files = dir("stocks")
all_files
closing_stock = list()
for(i in 1:length(all_files)){
  path = str_c("stocks/", all_files[i])
  parsed_stock = xmlParse(path)
  closing_stock[[i]] = xpathSApply(parsed_stock, "//Apple", getStock)
}



closing_stock = unlist(closing_stock)
closing_stock = data.frame(matrix(closing_stock, ncol = 2, byrow = T))
colnames(closing_stock) = c("date", "value")

closing_stock$date = as.Date(closing_stock$date, "%Y/%m/%d")
closing_stock$value = as.numeric(as.character(closing_stock$value))

plot(closing_stock$date, closing_stock$value, type = "l", main = "", ylab = "Closing stock", xlab = "Time")

a = 0
while(a < 3){
  a = a + 1
  print(a)
}

a = 0
while(TRUE){
  a = a + 1
  print(a)
  if(a >=3){
    break
  }
}

url = "http://www.example.com"

xpath_for_next_page = "//a[@class='NextPage']"

i = 1

current_document = getURL(url)
write(tmp, str_c(i, ".html"))

while(length(xpathSApply(current_document, xpath_for_next_page, xmlGetAttr, "href")) > 0){
  current_url = xpathSApply(current_document, xpath_for_next_page, xmlGetAttr, "href")
  current_document = getURL(current_url)
  write(current_document, str_c(i, ".html"))
  i = i + 1
  
}

files = str_c("stocks/", all_files)

getStock2 = function(file){
  parsedStock = xmlParse(file)
  closing_stock = xpathSApply(parsedStock, "//Apple/date | //Apple/close", xmlValue)
  closing_stock = as.data.frame(matrix(closing_stock, ncol = 2, byrow = T))
}


appleStocks = ldply(files, getStock2)
head(appleStocks, 3)

url = "http://www.buzzfeed.com"
parsed_page = htmlParse(url)
links = xpathSApply(parsed_page, "//a[@href]", xmlGetAttr, "href")
length(links)

collectHref = function(url){
  parsed_page = htmlParse(url)
  links = xpathSApply(parsed_page, "//[@href]", xmlGetAttr, "href")
  return(links)
}
buzzfeed = collectHref("http://www.buzzfeed.com")
length(buzzfeed)
slate = collectHref("http://www.slate.com")
length(slate)

collectHref = function(url, begins.http){
  if(!is.logical(begins.http)){
    stop"begins.http must be a logical value")
  }
  parsed_page = htmlParse(url)
  links = xpathSApply(parsed_page, "//a[@href]", xmlGetAttr, "href")
  if(begins.http == TRUE){
    links = links[str_detect(links, "^http")]
  }
  return(links)
}

buzzfeed = collectHref(url, begins.http = TRUE)
length(buzzfeed)
testPage = collectHref(url, begins.http = "TRUE")

collectHref = function(url, begins.http = TRUE){
  if(!is.logical(begins.http)){
    stop"begins.http must be a logical value")
  }
  parsed_page = htmlParse(url)
  links = xpathSApply(parsed_page, "//a[@href]", xmlGetAttr, "href")
  if(begins.http == TRUE){
    links = links[str_detect(links, "^http")]
  }
  return(links)
}

source("collectHref.r")
test_out = collectHref("http://www.buzzfeed.com")
length(test_out)

baseurl = "http://www.r-datacollection.com/materials/workflow/stocks"
links = str_c(baseurl, "/stocks_", 2003:2013, ".xml")

N = length(links)
for(i in 1:N){
  stocks = getURL(links[i])
  name = basename(links[i])
  write(stocks, file = str_c("stocks/", name))
  cat(i, "of", N, "\n")
}

for(i in 1:N){
  stocks = getURL(links[i])
  name = basename(links[i])
  write(stocks, file = str_c("stocks/", name))
  cat(i, "of", N, "-", name, "\n")
}

for(i in 1:30){
  if(i %% 10 == 0){
    cat(i, "of", 30, "\n")
  }
}

write("", "download.txt")
N = length(links)
for(i in 1:N){
  stocks = getURL(links[i])
  name = basename(links[i])
  write(stocks, file = str_c("stocks/", name))
  feedback = str_c(i, "of", N, "-", name, "\n", sep = " ")
  cat(feedback)
  write(feedback, "download.txt", append = T)
  write(nchar(stocks), "download.txt", append = T)
  write("------------\n", "download.txt", append = T)
}

progress_bar = txtProgressBar(min = 0, max = N, style = 3)
for(i in 1:N){
  stocks = getURL(links[i])
  name = basename(links[i])
  write(stocks, file = str_c("stocks/", name))
  setTxtProgressBar(progress_bar, i)
  Sys.sleep(1)
}

for(i in 1:N){
  tmp = getURL(website[i])
  write(tmp, str_c(str_replace(websites[i], "http://www.\\.", ""), ".html"))
}
cat("\a")

wrong_pages = c("http://www.bozzfeed.com", links)
for(i in 1:N){
  url = try(getURL(wrong_pages[i]))
  if(class(url) != "try_error"){
    name = basename(wrong_pages[i])
    write(url, name)
  }
}

collectHTML = function(url){
  html = getURL(url)
  write(html, basename(url))
}
write("", "error_log.txt")

for(i in 1:N){
  html = tryCatch(collectHTML(site404[i]), error = function(err){
    errMess = str_c("Not available - ", site404[i])
    write(str_c(errMess, "error_log.txt"))
  })
}

if(!file.exists("quotes")) dir.create("quotes")
time = str_replace_all(as.character(Sys.time()), ":", "_")
fname = str_c("quotes/rquote ", time, ".html")
url = "http://www.r-datacollection.com/materials/workflow/rQuotes.php"
download.file(url = url, destfile = fname)

