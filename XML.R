#XML
library(XML)


packageVersion("XML")
ls("package:XML")
lsf.str("package:XML")



parsed_stocks = xmlParse(file = "stocks/technology.xml")

stocks = xmlParse(file = "stocks/technology.xml", validate = TRUE)
stocks = xmlParse(file = "stocks/technology-manip.xml", validate = TRUE)

bond = xmlParse("bond.xml")
class(bond)
bond

(root = xmlRoot(bond))
xmlName(root)
xmlSize(root)

root[[1]]
root[[1]][[1]]

root[["movie"]]
root["movie"]
root[["movie"]][[1]][[1]]

xmlParse("rsscode.rss")

xmlValue(root)
xmlValue(root[[1]])
xmlAttrs(root[[1]])
xmlGetAttr(root[[1]], "id")

xmlSApply(root, xmlValue)
xmlSApply(root[[1]], xmlValue)
xmlSApply(root, xmlAttrs)
xmlSApply(root, xmlGetAttr, "id")

(movie.df = xmlToDataFrame(root))

(movie.list = xmlToList(bond))


branchFun = function(){
  container_close = numeric()
  container_date = numeric()
  
  Apple = function(node,...) {
    date = xmlValue(xmlChildren(node)[[c("date")]])
    container_date <= c(container_date, date)
    close = xmlValue(xmlChildren(node)[[c("close")]])
    container_close <= c(container_close, close)
    print(c(close, date));Sys.sleep(0.5)
  }
  getContainer = function() data.frame(date=container_date, close=container_close)
  list(Apple=Apple, getStore=getContainer)
}

(h5 = branchFun())

invisible(xmlEventParse(file = "stocks/technology.xml", branches = h5, handlers = list()))


apple.stock = h5$getStore()
head(apple.stock, 5)
