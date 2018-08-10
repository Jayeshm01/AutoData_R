
require(stringr)
require(XML)
require(RCurl)
require(RSQLite)

readLine = function(x) paste0(readLines(x), collapse="\n")

useRagent = "eddie@r-datacollection.com"


getFirstPage = function(keyword, forceDownload=F ){
  baseURL = "http://www.amazon.com/s/ref=nb_sb_noss_2?url=node%3D2407749011&field-keywords="
  
  fname = paste0(keyword, " SearchPage 0.html")
  if( !file.exists(fname) | forceDownload == T ){
    message("downloading first search page")
    url             = paste0(baseURL, keyword)
    firstSearchPage = getURL(url, useragent=useRagent)
    writeLines(firstSearchPage, fname)
  }else{
    message("loading first search page from disk")
    firstSearchPage = readLine(fname)
  }
  
  xpath = paste0('//span[@class="refinementLink" and text()="', keyword,'"]/../@href')
  
  restrictedSearchPageLink = xpathApply(htmlParse(firstSearchPage), xpath)
  restrictedSearchPageLink = unlist(as.character(restrictedSearchPageLink))
  
  restrictedSearchPageLink = paste0("http://www.amazon.com",
                                     restrictedSearchPageLink)
  
  restrictedSearchPageLink = paste0(restrictedSearchPageLink,
                                     "&sort=salesrank")
  restrictedSearchPageLink
  
  fname = paste0(keyword, " SearchPage 1.html")
  if( !file.exists(fname) | forceDownload == T ){
    message("downloading restricted search page")
    restrictedSearchPage = getURL(restrictedSearchPageLink, useragent=useRagent)
    writeLines(restrictedSearchPage, fname)
  }else{
    message("loading restricted search page from disk")
    restrictedSearchPage = readLine(fname)
  }
  
  return(restrictedSearchPage)
}
getNextSearchPage = function(PreviousSearchPage, fname="", forceDownload=F){
  if( !file.exists(fname) | forceDownload==T ){
    message("downloading next page")
    xpath = "//a[@class='pagnNext']/@href"
    nextPageLink = xpathApply( htmlParse(PreviousSearchPage), xpath)
    nextPageLink = paste0("http://www.amazon.com", nextPageLink)
    nextPage = getURL(nextPageLink, useragent=useRagent)
    writeLines(nextPage, fname)
  }else{
    message("loading next page from disk")
    nextPage = readLine(fname)
  }
  return(nextPage)
}
getSearchPages = function(keyword, n, forceDownload=F){
  SearchPages           = list()
  SearchPages[[1]]      = getFirstPage(keyword, forceDownload)
  while( length(SearchPages) < n ){
    i = length(SearchPages) + 1
    fname = paste0(keyword, " SearchPage ",i,".html")
    SearchPages[[ i ]] = getNextSearchPage(SearchPages[[i-1]], fname, forceDownload)
  }
  return(SearchPages)
}


extractTitles = function(SearchPages){
  extractTitle = function(x){
    unlist(xpathApply( htmlParse(x), "//h3/a/span", xmlValue)[1:24])
  }
  titles = unlist(lapply(SearchPages, extractTitle))
  names(titles) = NULL
  return(titles)
}


extractLinks = function(SearchPages){
  extractLink = function(x){
    unlist(xpathApply( htmlParse(x), "//h3/a", xmlAttrs)[1:24])
  }
  links = unlist(lapply(SearchPages, extractLink))
  names(links) = NULL
  return(links)
}

getProductPages = function(links, keyword, forceDownload=F){
  chunk = function(x,n) split(x, ceiling(seq_along(x)/n))
  Links = chunk(links,6)
  curl = getCurlHandle()
  ProductPages  = list()
  counter = 1 
  for(i in 1:length(Links)){
    drange = counter:(counter+length(Links[[i]])-1)
    kwords = keyword[drange]
    fnames = paste0(kwords, " ProductPage ", drange, ".html")
    if(any(!file.exists(fnames)) | forceDownload==T){
      message("\ndownloading ...")
      ProductPages = c( ProductPages, 
                         getURL( Links[[i]], curl = curl, useragent=useRagent) 
      )
      wl = function(text,con) writeLines(unlist(text),con)
      mapply( wl, text=ProductPages[drange], con=fnames) 
      Sys.sleep(0.5)
    }else{
      message("loading from disk ...")
      ProductPages = c(ProductPages, lapply(fnames, readLine))
    }
    message(paste(fnames,collapse="\n"))
    counter = counter+length(Links[[i]])
  }
  message("parsing product pages")
  ParsedProductPages = lapply(ProductPages, htmlParse)
  return(ParsedProductPages)
}

extractStars = function(ParsedProductPages){
  extractStar = function(x){
    tmp = xpathApply( x, 
                       "//span[@class='reviewCountTextLinkedHistogram']/@title",
                       as.character)
    tmp = unlist(tmp)[1]
    if(length(tmp)==0){
      x = xpathApply( x, 
                       "//span[contains(@class,'swSprite')]/@title", 
                       as.character)
      x = unlist(x)[1]
    }else{
      x = tmp
    }
    if( length(x)==0 ){
      x = NA
    }else{
      x = x[[1]]
      x = str_extract(x, "[[:digit:]]\\.?[[:digit:]]?")
    }
    return( as.numeric(x) )
  }
  stars = unlist(lapply(ParsedProductPages, extractStar))
  names(stars) = NULL
  return(stars)
}
extractASINs = function(ParsedProductPages){
  extractASIN = function(x){
    x = xpathApply( x, "//input[@id='ASIN']/@value", as.character)
    x = unlist(x)
    if( length(x)==0 ) x = NA
    return( x )
  }
  asins = unlist(lapply(ParsedProductPages, extractASIN))
  return(asins)
}

extractModels = function(ParsedProductPages){
  extractModel = function(x){
    xpath = "//li/b[contains(text(), 'Item model number')]/../text()"
    x = xpathApply( x, xpath, xmlValue)
    x = unlist(x)
    if( length(x)==0 ) x = NA
    return( x )
  }
  models = unlist(lapply(ParsedProductPages, extractModel))
  return(models)
}

extractRanks = function(ParsedProductPages){
  extractRank = function(x){
    x = xpathApply( x, "//li[@id='SalesRank']", xmlValue)
    x = unlist(x)
    x = str_extract(x,"#.*?in")
    x = str_replace_all(x,"[, in#]","")
    if( length(x)==0 ) x = NA
    return( as.numeric(x) )
  }
  ranks = unlist(lapply(ParsedProductPages, extractRank))
  return(ranks)
}

extractPrices = function(ParsedProductPages){
  extractPrice = function(x){
    tmp = xpathApply( x, '//span[@id="priceblock_ourprice"]', xmlValue )
    if(length(tmp)==0){
      x   = xpathApply( x, '//span[@id="actualPriceValue"]', xmlValue )
    }else{
      x = tmp
    }
    x = unlist(x)
    x = str_extract(x,"[[:digit:]]*\\.[[:digit:]]*")
    if( length(x)==0 ) x = NA
    return( as.numeric(x) )
  }
  prices = unlist(lapply(ParsedProductPages, extractPrice))
  return(prices)
}

