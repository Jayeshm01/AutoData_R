install.packages('tm')
install.packages('SnowballC')
install.packages('RWeka')
install.packages('RTexttools')
install.packages('topicmodels')


library(RCurl)
library(XML)
library(stringr)
library(tm)
library(SnowballC)
library(RWeka)
library(RTextTools)
library(topicmodels)

all_links = character()
new_results = 'government/announcements?keywords=&announcement_type_option=press-releases&topics[]=all&departments[]=all&world_locations[]=all&from_date=&to_date=01%2F07%2F2010'
signatures = system.file("CurlSSL", cainfo = "cacert.pem", package = "RCurl")
while(length(new_results) > 0){
  new_results = str_c("https://www.gov.uk/", new_results)
  results = getURL(new_results, cainfo = signatures)
  results_tree = htmlParse(results)
  all_links = c(all_links, xpathSApply(results_tree, "//li[@id]//a", xmlGetAttr, "href"))
  new_results = xpathSApply(results_tree, "//nav[@id='show-more-documents']//li[@class='next']//a", xmlGetAttr, "href")
}

all_links[1]
length(all_links)

for(i in 1:length(all_links)){
  url = str_c("https://www.gov.uk", all_links[i])
  tmp = getURL(url, cainfo = signatures)
  write(tmp, str_c("Press_Releases/", i, ".html"))
}

length(list.files("Press_Releases"))
list.files("Press_Releases")[1:3]

tmp = readLines("Press_Releases/1.html")
tmp = str_c(tmp, collapse = "")
tmp = htmlParse(tmp)
release = xpathSApply(tmp, "//div[@class='block-4'", xmlValue)

organisation = xpathSApply(tmp, "//span[@class='organisation lead']", xmlVAlue)
organisation
publication = xpathSApply(tmp, "//dd[@class='change-notes']", xmlValue)
publication

release_corpus = Corpus(VectorSource(release))

meta(release_corpus[[1]], "organisation") = organisation[1]
meta(release_corpus[[1]], "publication") = publication
meta(release_corpus[[1]])
n = 1
for(i in 2:length(list.files("Press_Releases/"))){
  tmp = readLines(str_c("Press_Releases/", i, ".html"))
  tmp = str_c(tmp, collapse = "")
  tmp = htmlParse(tmp)
  release = xpathSApply(tmp, "//div[@class='block-4']", xmlValue)
  organisation = xpathSApply(tmp, "//span[@class='organisation lead']", xmlValue)
  publication = xpathSApply(tmp, "//dd[@class='change-notes']", xmlValue)
  if(length(release) != 0){
    n = n + 1
    tmp_corpus = Corpus(VectorSource(release))
    release_corpus = c(release_corpus, tmp_corpus)
    meta(release_corpus[[n]], "organisation") = organisation[1]
    meta(release_corpus[[n]], "publication") = publication
  }
}
release_corpus

meta_data = prescindMeta(release_corpus, c("organisation", "publication"))
table(as.character(meta_data[, "organisation"]))

release_corpus = release_corpus[sFilter(release_corpus, "
                                         organisation == 'Department for Business, Innovation & Skills' |
                                         organisation == 'Department for Communities and Local Government' |
                                         organisation == 'Department for Environment, Food & Rural Affairs' |
                                         organisation == 'Foreign & Commonwealth Office' |
                                         organisation == 'Ministry of Defence' |
                                         organisation == 'Wales Office'")]
release_corpus
afgh = tm_filter(release_corpus, FUN = function(x) any(str_detect(x, "Afghanistan")))


tdm = TermDocumentMatrix(release_corpus)
tdm

release_corpus = tm_map(release_corpus, removeNumbers)

release_corpus = tm_map(release_corpus, str_replace_all, pattern = "[[:punct:]]", replacement = " ")

length(stopwords("en"))
stopwords("en")[1:10]
release_corpus = tm_map(release_corpus, removeWords, words = stopwords("en"))

release_corpus = tm_map(release_corpus, tolower)

release_corpus = tm_map(release_corpus, stemDocument)


tdm = TermDocumentMatrix(release_corpus)
tdm

tdm = removeSparseTerms(tdm, 1-(10/length(release_corpus)))
tdm

BigramTokenizer = function(x){
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}
tdm_bigram = TermDocumentMatrix(release_corpus, control = list(tokenize = BigramTokenizer))
tdm_bigram

findAssocs(tdm, "nuclear", .7)

dtm = DocumentTermMatrix(release_corpus)
dtm = removeSparseTerms(dtm, 1-(10/length(release_corpus)))
dtm

org_labels = unlist(prescindMeta(release_corpus, "organisation")[,2])
org_labels[1:3]

N = length(org_labels)
container = create_container(
  dtm,
  labels = org_labels,
  trainSize = 1:400,
  testSize = 401:N,
  virgin = F
)
slotNames(container)

svm_model = train_model(container, "SVM")
tree_model = train_model(container, "TREE")
maxent_model = train_model(container, "MAXENT")

svm_out = classify_model(container, svm_model)
tree_out = classify_model(container, tree_model)
maxent_out = classify_model(container, maxent_model)

head(svm_out)
head(tree_out)
head(maxent_out)

labels_out = data.frame(
  correct_label = org_labels[401:N],
  svm = as.character(svm_out[,1]),
  tree = as.character(tree_out[,1]),
  maxent = as.character(maxent_out[,1]),
  stringsAsFactors = F
)

table(labels_out[,1] == labels_out[,2])
table(labels_out[,1] == labels_out[,3])
table(labels_out[,1] == labels_out[,4])

prop.table(table(labels_out[,1] == labels_out[,4]))

short_corpus = release_corpus[c(
  which(tm_index(
    release_corpus,
    FUN = sFilter,
    s = "organisation == 'Ministry of Defence'"))[1:20],
  which(tm_index(
    release_corpus,
    FUN = sFilter,
    s = "organisation == 'Wales Office'"))[1:20],
  which(tm_index(
    release_corpus,
    FUN = sFilter,
    s = "organisation == 'Department for Environment, Food & Rural Affairs'"))[1:20]
)]

table(as.character(prescindMeta(short_corpus, "organisation")[,2]))

short_dtm = DocumentTermMatrix(short_corpus)
short_dtm = removeSparseTerms(short_dtm, 1-(5/length(short_corpus)))
rownames(short_dtm) = c(rep("Defence", 20), rep("Wales", 20), rep("Environment", 20))

dist_dtm = dist(short_dtm)
out = hclust(dist_dtm, method = "ward")
plot(out)

lda_out = LDA(dtm, 6)
posterior_lda = posterior(lda_out)
lda_topics = data.frame(t(posterior_lda$topics))
mean_topic_matrix = matrix(
  NA,
  nrow = 6,
  ncol = 6,
  dimnames = list(
    names(table(org_labels)),
    str_c("Topic_", 1:6)
  )
)
for(i in 1:6){
  mean_topic_matrix[i,] apply(lda_topics[, which(org_labels == rownames(mean_topic_matrix)[i])], 1, mean)
}
round(mean_topic_matrix, 2)

terms(lda_out, 10)

ctm_out = CTM(dtm, 6)
terms(ctm_out, 10)

posterior_ctm = posterior(ctm_out)
ctm_topics = data.frame(t(posterior_ctm$topics))

par(mfrow = c(2,3), cex.main = .8, pty = "s", mar = c(5, 5, 1, 1))
for(topic in 1:2){
  for(orga in names(table(org<_labels))){
    tmp.data = ctm_topics[topic, org_labels == orga]
    plot(
      1:ncol(tmp.data),
      sort(as.numeric(tmp.data)),
      type = "l",
      ylim = c(0, 1),
      xlab = "Press releases",
      ylab = str_c("Posterior probability, topic ", topic),
      main = str_replace(orga, "Department for", "")
    )
  }
}

