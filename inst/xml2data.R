
library("XML")

options(stringsAsFactors = FALSE)


create_Rd <- function(object, title, description, seealso = NULL) {
  name <- as.character(substitute(object))
  rd <- promptData(object, filename = NA, name = name)

  rd$title <- sprintf("\\title{%s}", title)
  rd$description <- c("\\description{", description, "}")

  if ( !is.null(seealso) )
    rd$seealso <- c("\\seealso{",
                    paste("\\code{\\link{", seealso, "}}", sep = "", collapse = ", "),
                    "}")

  rd$source <- c("\\source{", "Screen scraped from the ISIPTA website \\url{http://www.sipta.org/}; see directories \\code{scraper} and \\code{xml} for the screen scrapers and the raw data.", "}")

  rd$details <- NULL
  rd$references <- NULL

  ## Rd file:
  cat(unlist(rd), file = sprintf("../man/%s.Rd", name), sep = "\n")
}



### Conferencs: ######################################################

xmlLocation2data <- function(x) {
  c(country_code = xmlValue(x[["country"]][["code"]]),
    country_name = xmlValue(x[["country"]][["name"]]),
    city = xmlValue(x[["city"]][["name"]]),
    city_lat = xmlValue(x[["city"]][["latitude"]]),
    city_lon = xmlValue(x[["city"]][["longitude"]]),
    university = xmlValue(x[["university"]][["name"]]),
    department = xmlValue(x[["university"]][["department"]]))
}


xml2conferences <- function() {
  extract <- function(xmlfile) {
    tree <- xmlTreeParse(xmlfile, useInternalNodes = TRUE)
    proc <- xmlRoot(tree)

    year <- c(year = xmlValue(proc[["year"]]))
    location <- xmlLocation2data(proc[["conference"]][["location"]])
    date <- c(date_start = xmlValue(proc[["conference"]][["date"]][["start"]]),
              date_end = xmlValue(proc[["conference"]][["date"]][["end"]]))

    free(tree)

    c(year, location, date)
  }

  xmlfiles <- list.files("xml/",
                         pattern = "isipta.*\\.xml",
                         full.names = TRUE)

  conferences <- lapply(xmlfiles, extract)
  conferences <- do.call(rbind, conferences)

  conferences[conferences == ""] <- NA

  conferences <- as.data.frame(conferences)
  conferences <- within(conferences, {
    year <- as.integer(year)
    date_start <- as.Date(date_start)
    date_end <- as.Date(date_end)
    country_name <- factor(country_name)
    country_code <- factor(country_code)
    city_lat <- as.numeric(city_lat)
    city_lon <- as.numeric(city_lon)
  })

  conferences
}


conferences <- xml2conferences()
save(conferences, file = "../data/conferences.RData")

create_Rd(conferences,
          "ISIPTA conference facts",
          "Dates, locations, etc. about the ISIPTA conferences.",
          "papers")



### Papers: ##########################################################

xml2papers <- function() {
  extract <- function(xmlfile) {
    tree <- xmlTreeParse(xmlfile, useInternalNodes = TRUE)
    proc <- xmlRoot(tree)

    year <- xmlValue(proc[["year"]])
    papers <- getNodeSet(proc, "//paper",
                         fun = function(paper) {
                           id <- xmlValue(paper[["id"]])
                           title <- xmlValue(paper[["title"]])
                           c(year = year,
                             id = sprintf("%s%s", year, id),
                             title = title)
                         })

    free(tree)

    do.call(rbind, papers)
  }

  xmlfiles <- list.files("xml/",
                         pattern = "isipta.*\\.xml",
                         full.names = TRUE)

  papers <- lapply(xmlfiles, extract)
  papers <- do.call(rbind, papers)
  papers <- as.data.frame(papers)
  papers <- within(papers, {
    year <- as.integer(year)
    id <- as.integer(id)
  })

  papers
}


papers <- xml2papers()
save(papers, file = "../data/papers.RData")

create_Rd(papers,
          "Titles of the ISIPTA papers",
          "Titles of the ISIPTA papers; abstracts are available in the XML file (see directory \\code{xml}.",
          c("papers_authors", "authors_locations", "papers_keywords"))




### Authors: #########################################################

xml2papers_authors <- function() {
  extract <- function(xmlfile) {
    tree <- xmlTreeParse(xmlfile, useInternalNodes = TRUE)
    proc <- xmlRoot(tree)

    year <- xmlValue(proc[["year"]])
    authors <- getNodeSet(proc, "//paper",
                          fun = function(paper) {
                            id <- xmlValue(paper[["id"]])

                            authors <- getNodeSet(paper[["authors"]], ".//author",
                                                  fun = function(x) {
                                                    xmlValue(x[["name"]])
                                                  })

                            data.frame(year = year,
                                       id = sprintf("%s%s", year, id),
                                       author = unlist(authors))
                          })

    free(tree)

    do.call(rbind, authors)
  }

  xmlfiles <- list.files("xml/",
                         pattern = "isipta.*\\.xml",
                         full.names = TRUE)

  authors <- lapply(xmlfiles, extract)
  authors <- do.call(rbind, authors)
  authors <- as.data.frame(authors)
  authors <- within(authors, {
    year <- as.integer(year)
    id <- as.integer(id)
    author <- factor(author)
  })

  authors
}


papers_authors <- xml2papers_authors()
save(papers_authors, file = "../data/papers_authors.RData")

create_Rd(papers_authors,
          "Authors of the ISIPTA papers",
          "Authors of the ISIPTA papers; the names are normalized to ASCII characters.",
          c("papers", "authors_locations", "papers_keywords"))



### Keywords: ########################################################

xml2keywords <- function() {
  extract <- function(xmlfile) {
    tree <- xmlTreeParse(xmlfile, useInternalNodes = TRUE)
    proc <- xmlRoot(tree)

    year <- xmlValue(proc[["year"]])
    keywords <- getNodeSet(proc, "//paper",
                           fun = function(paper) {
                             id <- xmlValue(paper[["id"]])

                             keywords <- xpathSApply(paper[["keywords"]],
                                                     ".//keyword",
                                                     xmlValue)

                             data.frame(year = year,
                                        id = sprintf("%s%s", year, id),
                                        keyword = keywords)
                           })

    free(tree)

    do.call(rbind, keywords)
  }

  xmlfiles <- list.files("xml/",
                         pattern = "isipta.*\\.xml",
                         full.names = TRUE)

  keywords <- lapply(xmlfiles, extract)
  keywords <- do.call(rbind, keywords)
  keywords <- as.data.frame(keywords)
  keywords <- within(keywords, {
    year <- as.integer(year)
    id <- as.integer(id)
    #keyword <- factor(keyword)
    keyword <- iconv(keyword, from = "UTF-8", to = "latin1")
  })

  keywords
}

papers_keywords <- xml2keywords()
save(papers_keywords, file = "../data/papers_keywords.RData")

create_Rd(papers_keywords,
          "Keywords of the ISIPTA papers",
          "Keywords of the ISIPTA papers.",
          c("papers", "authors_locations", "papers_authors"))



### Locations: #######################################################

xml2authors_locations <- function() {
  extract <- function(xmlfile) {
    tree <- xmlTreeParse(xmlfile, useInternalNodes = TRUE)
    proc <- xmlRoot(tree)

    year <- xmlValue(proc[["year"]])
    locations <- getNodeSet(proc, "//author",
                            fun = function(author) {
                              c(year = year,
                                author = xmlValue(author[["name"]]),
                                xmlLocation2data(author[["location"]]))
                            })

    free(tree)

    do.call(rbind, locations)
  }

  xmlfiles <- list.files("xml/",
                         pattern = "geoloc_authors.*\\.xml",
                         full.names = TRUE)

  locations <- lapply(xmlfiles, extract)
  locations <- do.call(rbind, locations)

  locations[locations == "NA"] <- NA
  locations[locations == "N/A"] <- NA

  locations <- as.data.frame(locations)
  locations <- within(locations, {
    year <- as.integer(year)
    author <- factor(author)
    country_name <- factor(country_name)
    country_code <- factor(country_code)
    city <- factor(city)
    city_lat <- as.numeric(city_lat)
    city_lon <- as.numeric(city_lon)
  })

  locations
}

authors_locations <- xml2authors_locations()
save(authors_locations, file = "../data/authors_locations.RData")

create_Rd(authors_locations,
          "Estimated location of the authors",
          "Estimated (by the authors' email domains and their geotargeting using GeoIP with databases by MaxMind \\url{http://www.maxmind.com/app/ip-location}) location of the ISIPTA authors.",
          c("papers_authors"))




