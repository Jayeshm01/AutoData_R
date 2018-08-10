
source("amazon_func.r")

if(!file.exists("dataFull") & grepl("dataFull",basename(getwd()))) dir.create("dataFull")
setwd("dataFull")
forceDownload = F
KeyWords = c("Apple", "BlackBerry", "HTC",
              "LG", "Motorola", "Nokia", "Samsung")
n = 5
SearchPageList = NULL
SearchPagesKeywords = NULL
for(i in seq_along(KeyWords)){
  message(KeyWords[i])
  SearchPageList = c(SearchPageList, 
                      getSearchPages(KeyWords[i], n, forceDownload)
  )
  SearchPagesKeywords = c( SearchPagesKeywords, rep(KeyWords[i], n) )
}

titles  = extractTitles(SearchPageList)
links   = extractLinks(SearchPageList)


if(!file.exists("phones.Rdata") | forceDownload==T){
  brands = rep(KeyWords,each=n*24)
  productPages = getProductPages(links, brands, forceDownload)
  
  stars  = extractStars(productPages)
  asins  = extractASINs(productPages)
  models = extractModels(productPages)
  ranks  = extractRanks(productPages)
  prices = extractPrices(productPages)
  
  fnames = paste0(brands, " ProductPage ", 1:(length(KeyWords)*n*24),".html" )
  
  phones = data.frame(   brands, prices, stars, ranks, asins, models, titles, links, fnames,
                          timestamp=file.info(fnames)$ctime,
                          stringsAsFactors=F)
  
  save(phones, stars, asins, models, ranks, prices, brands, timestamp, titles, fnames,
       file="phones.Rdata")
  
}else{
  load("phones.Rdata")
}
phonesClean = phones[complete.cases(phones),]

phonesClean = phonesClean[!duplicated(phonesClean$asins),]

mprices = rep(NA,length(phonesClean$prices))
for(i in seq_along(phonesClean$models)){
  mprices[i] = mean(phonesClean$prices[phonesClean$models[i]==phonesClean$models], na.rm=T)
}
phonesClean$prices = round(mprices,2)

phonesClean = phonesClean[!duplicated(phonesClean$models),]
plotResults = function(X, title="") {
  Prices = X$prices
  Stars  = X$stars
  Ranks  = X$ranks
  plot(  Prices, Stars, pch=20, cex=10, col="white",
         ylim=c(1,5),xlim=c(0,1000), main=title, 
         cex.main=2, cex.axis=2, xaxt="n")
  axis(1, at=c(0,500,1000),labels=c(0,500,1000),cex.axis=2)
  abline(v=seq(0,1000,100),col="grey")
  abline(h=seq(0,5,1),col="grey")
  points(Prices, Stars, col=rgb(0,0,0,0.2), pch=20, cex=7)
  index = order(Ranks)[1:5]
  abline(v=Prices[index],col="black")
  abline(h=Stars[index],col="black")
  points(Prices[index], Stars[index], col=rgb(1,1,1,1), pch=20, cex=2)
}


pdf("ranks.pdf", width=12, height=6)

par(mfrow=c(2,4))
par(mar=c(3,2,2,1))
par(oma=c(3, 4, 0, 1))

plotResults(phones, title="all")
for(i in seq_along(KeyWords)){
  plotResults( phones[phones$brands==KeyWords[i], ], title=KeyWords[i] )
}

mtext("costumer ratings\n",2,outer=T,cex=1.5)
mtext("prices",1,outer=T,cex=1.5)
dev.off()



