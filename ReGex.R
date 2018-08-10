#install.packages('tau')

library(stringr)
library(XML)
library(RCurl)
library(tau)

raw.data = "555-1239Moe Szyslak(636) 555-0113Burns, C. Montgomery555-6542Rev. Timothy Lovejoy555 8904Ned Flanders636-555-3226Simpson, Homer5543642Dr. Julius Hibbert"

name = unlist(str_extract_all(raw.data, "[[:alpha:]., ]{2,}"))
name
phone = unlist(str_extract_all(raw.data, "\\(?(\\d{3})?\\)?(-| )?\\d{3}(-| )?\\d{4}"))
phone
data.frame(name = name, phone = phone)

example.obj = "1. A small sentence. - 2. Another tiny sentence."

str_extract(example.obj, "small")
str_extract(example.obj, "banana")
unlist(str_extract_all(example.obj, "sentence"))

out = str_extract_all(c("text", "manipulation", "basics"), "a")
out
str_extract(example.obj, "small")
str_extract(example.obj, "SMALL")
str_extract(example.obj, ignore.case("SMALL"))

unlist(str_extract_all(example.obj, "en"))
str_extract(example.obj, "mall sent")

str_extract(example.obj, "2")
str_extract(example.obj, "^2")
unlist(str_extract_all(example.obj, "sentence$"))

unlist(str_extract_all(example.obj, "tiny|sentence"))

str_extract(example.obj, "sm.ll")

str_extract(example.obj, "sm[abc]ll")

str_extract(example.obj, "sm[a-p]ll")

unlist(str_extract_all(example.obj, "[uvw. ]"))

unlist(str_extract_all(example.obj, "[[:punct:]]"))

unlist(str_extract_all(example.obj, "[:punct:]"))

unlist(str_extract_all(example.obj, "[AAAAA]"))

str_extract("François Hollande", "Fran[a-z]ois")
str_extract("François Hollande", "Fran[[:alpha:]]ois")

unlist(str_extract_all(example.obj, "[[:punct:]ABC]"))

unlist(str_extract_all(example.obj, "[^[:alnum:]]"))

str_extract(example.obj, "s[[:alpha:]][[:alpha:]][[:alpha:]]")
str_extract(example.obj, "s[[:alpha:]]{3}l")

str_extract(example.obj, "A.+sentence")
str_extract(example.obj, "A.+?sentence")

unlist(str_extract_all(example.obj, "(.en){1,5}"))
unlist(str_extract_all(example.obj, ".en{1,5}"))

unlist(str_extract_all(example.obj, "\\."))
unlist(str_extract_all(example.obj, fixed(".")))

unlist(str_extract_all(example.obj, "\\w+"))

unlist(str_extract_all(example.obj, "e\\>"))
unlist(str_extract_all(example.obj, "e\\b"))

str_extract(example.obj, "([[:alpha:]]).+?\\1")
str_extract(example.obj, "(\\<[b-z]+\\>).+?\\1")

raw.data = "555-1239Moe Szyslak(636) 555-0113Burns, C. Montgomery555-6542Rev. Timothy Lovejoy555 8904Ned Flanders636-555-3226Simpson, Homer5543642Dr. Julius Hibbert"

name = unlist(str_extract_all(raw.data, "[[:alpha:]., ]{2,}"))
name
phone = unlist(str_extract_all(raw.data, "\\(?(\\d{3})?\\)?(-| )?\\d{3}(-| )?\\d{4}"))
phone

str_extract(example.obj, "tiny")
str_extract_all(example.obj, "[[:digit:]]")

str_locate(example.obj, "tiny")

str_sub(example.obj, start = 35, end = 38)
str_sub(example.obj, 35, 38) = "huge"
example.obj

str_replace(example.obj, pattern = "huge", replacement = "giant")

unlist(str_split(example.obj, "-"))
as.character(str_split_fixed(example.obj, "[[:blank:]]", 5))

char.vec = c("this", "and this", "and that")

str_detect(char.vec, "this")

str_count(char.vec, "this")
str_count(char.vec, "\\w+")

dup.obj = str_dup(char.vec, 3)
dup.obj

length.char.vec = str_length(char.vec)
length.char.vec

char.vec = str_pad(char.vec, width = max(length.char.vec), side = "both", pad = " ")
char.vec
char.vec = str_trim(char.vec)
char.vec

cat(str_c(char.vec, collapse = "\n"))
str_c("text", "manipulation", sep = " ")
str_c("text", c("manipulation", "basics"), sep = " ")

agrep("Barack Obama", "Barack H. Obama", max.distance = list(all = 3))
agrep("Barack Obama", "Michelle Obama", max.distance = list(all = 3))

pmatch(c("and this", "and that", "and these", "and those"), char.vec)

make.unique(c("a", "b", "a", "c", "b", "a"))

load("episodes.Rdata")

grep("Homer", episodes$title[1:10], value = T)
grepl("Homer", episodes$title[1:10])

iffer1 = grepl("Homer", episodes$title)
iffer2 = grepl("Lisa", episodes$title)
iffer = iffer1 & iffer2
episodes$title[iffer]

grepall = function(pattern, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE, value = FALSE, logic = FALSE){
  
  if(length(pattern) == 0 | length(x) == 0){
    warning("Length of pattern or data equals zero.")
    return(NULL)
  }
  
  indices = sapply(pattern, grepl, x, ignore.case, perl, fixed, useBytes)
  index = apply(indices, 1, all)
  
  if(logic == T) return(index)
  if(value == F) return((1:length(x))[index])
  if(value == T) return(x[index])
}

grepall(c("Lisa", "Homer"), episodes$title)
grepall(c("Lisa", "Homer"), episodes$title, value = T)

Sys.getlocale()

small.frogs = "Små grodorna, små grodorna är lustiga att se."
small.frogs

small.frogs.utf8 = iconv(small.frogs, from = "windows-1252", to = "UTF-8")
small.frogs.utf8

Encoding(small.frogs.utf8) = "windows-1252"
small.frogs.utf8

sample(iconvlist(), 10)

enc.test = getURL("http://www.sciencemag.org/")
unlist(str_extract_all(enc.test, "<meta.+?>"))

is.locale(small.frogs)
is.ascii(small.frogs)
