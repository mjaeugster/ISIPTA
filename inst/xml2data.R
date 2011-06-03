
library("XML")

options(stringsAsFactors = FALSE)

extract_coauthors <- function(xmlfile) {
  tree <- xmlTreeParse(xmlfile, useInternalNodes = TRUE)
  proc <- xmlRoot(tree)
  year <- xmlValue(proc[["year"]])
  coauthors <- getNodeSet(xmlRoot(proc), "//paper",
                          fun = function(paper) {
                            id <- xmlValue(paper[["id"]])
                            authors <- sapply(getNodeSet(paper[["authors"]], ".//name"),
                                              xmlValue)
                            data.frame(year = year, paperid = id, author = authors)
                          })
  free(tree)

  do.call(rbind, coauthors)
}

xmlfiles <- list.files("xml/", pattern = ".xml", full.names = TRUE)

coauthors <- lapply(xmlfiles, extract_coauthors)
coauthors <- do.call(rbind, coauthors)
coauthors$paperid <- factor(paste(coauthors$year, coauthors$paperid, sep = ""))
coauthors$year <- ordered(coauthors$year, levels = seq(1999, 2009, by = 2))

save(coauthors, file = "../data/coauthors.Rdata")
