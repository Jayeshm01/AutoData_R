namesScrape = function(phonename, update.file = FALSE) {
  
  phonename = tolower(phonename)
  
  x = c("stringr", "RCurl", "XML")
  lapply(x, require, character.only=T)
  
  dir.create(str_c("phonebook_", phonename), showWarnings = FALSE)
  filename = str_c("phonebook_", phonename, "/phonebook_", phonename, ".html")
  if (file.exists(filename) & update.file == FALSE) {
    message("Data already scraped; using data from ", file.info(filename)$mtime)
  } else {
    
    tb = getForm("http://www.dastelefonbuch.de/",
                  .params = c(kw = phonename, cmd = "search", ao1 = "1", reccount = "2000"))
    write(tb, file = filename)
  }
}