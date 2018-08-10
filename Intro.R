#case Study

library(stringr)
library(XML)
library(maps)


#heritage_parsed = htmlParse("https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger", encoding = "UTF-8")
#tables = readHTMLTable(heritage_parsed, stringsAsFactors = FALSE)

heritage_parsed = htmlParse("worldheritagedanger.html",encoding="UTF-8")
tables = readHTMLTable(heritage_parsed, stringsAsFactors = FALSE)
danger_table = readHTMLTable(heritage_parsed, stringsAsFactors = FALSE) 


danger_table = tables[[2]]
names(danger_table)


danger_table = danger_table[,c(1,3,4,6,7)]
colnames(danger_table) = c("name","locn","crit","yins","yend")
danger_table$name[1:3]


danger_table$crit = ifelse(str_detect(danger_table$crit, "Natural")==T, "nat", "cult")


danger_table$yins = as.numeric(danger_table$yins)
danger_table$yend
yend_clean = unlist(str_extract_all(danger_table$yend, "[[:digit:]]{4}$"))
danger_table$yend = as.numeric(yend_clean)
danger_table$locn[c(1,3,5)]


reg_y = "[/][ -]*[[:digit:]]*[.]*[[:digit:]]*[;]"
reg_x = "[;][ -]*[[:digit:]]*[.]*[[:digit:]]*"
y_coords = str_extract(danger_table$locn, reg_y)
(y_coords = as.numeric(str_sub(y_coords, 3, -2)))
danger_table$y_coords = y_coords
x_coords = str_extract(danger_table$locn, reg_x)
(x_coords = as.numeric(str_sub(x_coords, 3, -1)))
danger_table$x_coords = x_coords
danger_table$locn = NULL



pch = ifelse(danger_table$crit == "nat", 19, 2)
map("world", col = "darkgrey", lwd = .5, mar = c(0.1,0.1,0.1,0.1))
points(danger_table$x_coords, danger_table$y_coords, pch = pch, col = "black", cex = .8)
box()


table(danger_table$crit)



hist(danger_table$yend, freq=TRUE, xlab="Year when site was put on the list of endangered sites", main="")
box()



duration = danger_table$yend - danger_table$yins

par(oma=c(0,0,0,0))
par(mar=c(4,4,1,.5))
hist(duration, freq=TRUE, xlab="Years it took to become an endangered site", main="")
box()



