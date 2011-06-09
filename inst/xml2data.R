
library("XML")

options(stringsAsFactors = FALSE)



### Coauthors: #######################################################

extract_coauthors <- function(xmlfile) {
  tree <- xmlTreeParse(xmlfile, useInternalNodes = TRUE)
  proc <- xmlRoot(tree)
  year <- xmlValue(proc[["year"]])

  coauthors <- getNodeSet(proc, "//paper",
                          fun = function(paper) {
                            id <- xmlValue(paper[["id"]])
                            authors <- sapply(getNodeSet(paper[["authors"]], ".//name"),
                                              xmlValue)
                            data.frame(year = year, paperid = id, author = authors)
                          })
  free(tree)

  do.call(rbind, coauthors)
}


xmlfiles <- list.files("xml/", pattern = "isipta.*\\.xml", full.names = TRUE)

coauthors <- lapply(xmlfiles, extract_coauthors)
coauthors <- do.call(rbind, coauthors)
coauthors$paperid <- factor(paste(coauthors$year, coauthors$paperid, sep = ""))
coauthors$year <- ordered(coauthors$year, levels = seq(1999, 2009, by = 2))

save(coauthors, file = "../data/coauthors.Rdata")



### Locations: #######################################################

extract_locations <- function(xmlfile) {
  tree <- xmlTreeParse(xmlfile, useInternalNodes = TRUE)
  proc <- xmlRoot(tree)
  year <- xmlValue(proc[["year"]])

  locations <- getNodeSet(proc, "//author",
                          fun = function(author) {
                            data.frame(year = year,
                                       author = xmlValue(author[["name"]]),
                                       latitude = xmlValue(author[["location"]][["city"]][["latitude"]]),
                                       longitude = xmlValue(author[["location"]][["city"]][["longitude"]]))
                          })

  free(tree)

  do.call(rbind, locations)
}


xmlfiles <- list.files("xml/", pattern = "geoloc_authors.*\\.xml", full.names = TRUE)

locations <- lapply(xmlfiles, extract_locations)
locations <- do.call(rbind, locations)
locations$latitude <- as.numeric(locations$latitude)
locations$longitude <- as.numeric(locations$longitude)
locations$year <- ordered(locations$year, levels = seq(1999, 2009, by = 2))

save(locations, file = "../data/locations.Rdata")
