#install.packages('igraph')

library(RCurl)
library(XML)
library(httr)
library(stringr)
library(igraph)

dir.create("Bills_111/")

for(i in 1:4059){
  url = str_c(
    "http://thomas.loc.gov/cgi-bin/bdquery/D?d111:", 
    i, 
    ":./list/bss/d111SN.lst:@@@P"
  )
  bill.result = getURL(url)
  write(
    bill.result, str_c(
      "Bills_111/Bill_111_S", 
      i, 
      ".html"
    )
  )
  cat(i, "\n")
}

sponsor.regex = "FLD003\\+@4\\(\\(@1\\([[:alpha:]+.]+"
cosponsor.regex = "FLD004\\+@4\\(\\(@1\\([[:alpha:]+.]+"

html.source = readLines("Bills_111/Bill_111_S1.html")
sponsor = str_extract(html.source, sponsor.regex)
(sponsor = sponsor[!is.na(sponsor)])
cosponsors = unlist(str_extract_all(html.source, cosponsor.regex))
cosponsors[1:3]
length(cosponsors)

clean.up = function(x){
  name = str_extract(x, "[[:alpha:]+.]+$")
  name = str_replace_all(name, fixed("++"), ", ")
  name = str_replace_all(name, fixed("+"), " ")
  name = str_trim(str_replace(name, "Sen", ""))    
  return(name)
}

clean.up(sponsor)
clean.up(cosponsors)

error.collection = list()
sponsor.list = list()
for(i in 1:4059){
  html.source = readLines(str_c("Bills_111/Bill_111_S", i, ".html"))
  sponsor = unlist(str_extract_all(html.source, sponsor.regex))
  sponsor = sponsor[!is.na(sponsor)]
  sponsor = clean.up(sponsor)
  cosponsors = unlist(str_extract_all(html.source, cosponsor.regex))
  cosponsors = clean.up(cosponsors)
  
  sponsor.list[[str_c("S.", i)]] = list(sponsor = sponsor, cosponsors = cosponsors)
  
  fail.safe = str_extract(html.source, "COSPONSORS?\\(([[:digit:]]{1,3}|S)\\)")
  fail.safe = fail.safe[!is.na(fail.safe)]
  if(length(fail.safe) == 0){
    error.collection[[length(error.collection) + 1]] = c(i, "String - COSPONSOR - not found")
  }
  if(fail.safe == "COSPONSOR(S)"){
    if(length(cosponsors) > 0){
      error.collection[[length(error.collection) + 1]] = c(i, "Found cosponsors where there should be none")
    }    
  }else{
    right.number = str_extract(fail.safe, "[[:digit:]]+")
    if(length(cosponsors) != right.number){
      error.collection[[length(error.collection) + 1]] = c(i, "Did not find the right number of cosponsors")
    }
  }
  if(is.na(sponsor)){
    error.collection[[length(error.collection) + 1]] = c(i, "No sponsors")
  }
  if(length(sponsor) > 1){
    error.collection[[length(error.collection) + 1]] = c(i, "More than one sponsor")
  }
}
length(error.collection)

for(i in 1:length(error.collection)){
  bill.number = as.numeric(error.collection[[i]][1])
  html.source = readLines(str_c("Bills_111/Bill_111_S", bill.number, ".html"))
  
  count.withdrawn = unlist(
    str_extract_all(
      html.source, 
      "\\(withdrawn - [[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}\\)"
    )
  ) 
  
  sponsor.list[[str_c("S.", bill.number)]]$cosponsors = sponsor.list[[str_c("S.", bill.number)]]$cosponsors[1:(length(sponsor.list[[str_c("S.", bill.number)]]$cosponsors) - length(count.withdrawn))]
}

all.senators = unlist(sponsor.list)
all.senators = unique(all.senators)
all.senators = sort(all.senators)
head(all.senators)

sponsor.matrix = matrix(NA, nrow = 4059, ncol = length(all.senators))
colnames(sponsor.matrix) = all.senators
rownames(sponsor.matrix) = paste("S.", seq(1, 4059), sep ="")
sponsor.matrix[30:35, 31:34]

for(i in 1:length(sponsor.list)){
  sponsor.matrix[i, which(all.senators == sponsor.list[[i]]$sponsor)] = "Sponsor"
  if(length(sponsor.list[[i]]$cosponsors) > 0){
    for(j in 1:length(sponsor.list[[i]]$cosponsors)){
      sponsor.matrix[i, which(all.senators == sponsor.list[[i]]$cosponsors[j])] = "Cosponsor"
    }
  }
}
sponsor.matrix[30:35,31:34]

form.page = getURL("http://bioguide.congress.gov/biosearch/biosearch.asp")
write(form.page, "form_page.html")

form.page = str_c(readLines("form_page.html"), collapse = "")
destination = str_extract(form.page, "<form.+?>")
cat(destination)
form = str_extract(form.page, "<form.+?</form>")
cat(str_c(unlist(str_extract_all(form, "<INPUT.+?>")), collapse = "\n"))
cat(str_c(unlist(str_extract_all(form, "<SELECT.+?>")), collapse = "\n"))

senator.site = POST(
  "http://bioguide.congress.gov/biosearch/biosearch1.asp", 
  body = list(
    lastname = "", 
    firstname = "", 
    position = "Senator", 
    state = "", 
    party = "", 
    congress = "111"
  ), 
  multipart = F
)
write(content(senator.site, as = 'text'), "senators.html")

senator.site = readLines("senators.html")
senator.site = htmlParse(senator.site)
senators = readHTMLTable(senator.site)[[2]]
head(senators)

senators$match.names = senators[,1]
senators$match.names = tolower(senators$match.names)
senators$match.names = str_extract(senators$match.names, "[[:alpha:]]+")

all.senators.dat = data.frame(all.senators)
all.senators.dat$match.names = str_extract(all.senators.dat$all.senators, "[[:alpha:]]+")
all.senators.dat$match.names = tolower(all.senators.dat$match.names)

senators = merge(all.senators.dat, senators, by = "match.names")
senators[,2] = as.character(senators[,2])
senators[,3] = as.character(senators[,3])
senators[,2] = tolower(senators[,2])
senators[,3] = tolower(senators[,3])

allDup = function(x){
  duplicated(x) | duplicated(x, fromLast = TRUE)
} 

dup.senators = senators[allDup(senators[,1]),]
senators = senators[rownames(senators) %in% rownames(dup.senators) == F,]

dup.senators[str_detect(dup.senators[,3], "\\("), 3] = str_replace_all(dup.senators[str_detect(dup.senators[,3], "\\("), 3], ", .+?\\(", ", ")
dup.senators[str_detect(dup.senators[,3], "\\)"), 3] = str_replace_all(dup.senators[str_detect(dup.senators[,3], "\\)"), 3], "\\)", "")

for(i in nrow(dup.senators):1){
  if(str_detect(dup.senators[i, 2], str_extract(dup.senators[i, 3], "[^,][[:alpha:] .]+?$")) == F){
    dup.senators = dup.senators[-i,]
  }
}

senators = rbind(senators, dup.senators)
senators$rownames = as.numeric(rownames(senators))
senators = senators[order(senators$rownames),]
dim(senators)

colnames(sponsor.matrix) = senators$all.senators

edgelist.sponsors = matrix(NA, nrow = 0, ncol = 2)
for(i in 1:nrow(sponsor.matrix)){
  if(length(which(!is.na(sponsor.matrix[i,]))) > 1){
    edgelist.sponsors = rbind(
      edgelist.sponsors, 
      t(combn(colnames(sponsor.matrix)[which(!is.na(sponsor.matrix[i,]))], 2))
    )
  }
}

sponsor.network = graph.edgelist(edgelist.sponsors, directed = F)

result = matrix(
  NA, 
  ncol = ncol(sponsor.matrix), 
  nrow = 2, 
  dimnames = list(
    c("Sponsor", "Cosponsor"), 
    colnames(sponsor.matrix)
  )
)
for(i in 1:ncol(sponsor.matrix)){
  result[1, i] = sum(sponsor.matrix[, i] == "Cosponsor", na.rm = T)
  result[2, i] = sum(sponsor.matrix[, i] == "Sponsor", na.rm = T)
}
result = t(result)

adj.sponsor = get.adjacency(sponsor.network)

adj.sponsor[lower.tri(adj.sponsor)] = 0
adj.sponsor[1:6, 1:6]

min(sort(adj.sponsor, decreasing = T)[1:10])
max.indices = which(adj.sponsor >= min(sort(adj.sponsor, decreasing = T)[1:10]), arr.in = T)
export.names = matrix(NA, ncol = 2, nrow = 10)
for(i in 1:nrow(max.indices)){
  export.names[i, 1] = rownames(adj.sponsor)[max.indices[i,1]]
  export.names[i, 2] = colnames(adj.sponsor)[max.indices[i,2]]
}

E(sponsor.network)$weight = 1
sponsor.network.weighted = simplify(sponsor.network)
head(E(sponsor.network.weighted)$weight)

plot.sponsor = sponsor.network.weighted
plot.sponsor = delete.edges(plot.sponsor, which(E(plot.sponsor)$weight < (mean(E(plot.sponsor)$weight) + sd(E(plot.sponsor)$weight))))
plot(plot.sponsor)
