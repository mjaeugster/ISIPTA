### Estimate the geolocation of the authors.

source("../scraper/xml.R")
source("domains.R")


GEOLOC_DOMAINS <- local({
  raw <- readLines("../xml/geoloc_domains.xml", encoding = "UTF-8")
  xmlTreeParse(raw, useInternalNodes = TRUE)
})


geolocation_by_hand <- function(author, year) {
  NULL
}


geolocation_by_domain <- function(author) {
  d <- domain(author[[2]])

  if ( is.null(d) )
    return(NULL)

  d <- basedomain(d)

  path <- sprintf("//domain[name='%s']/location", d)
  GEOLOC_DOMAINS <- local({
    raw <- readLines("../xml/geoloc_domains.xml", encoding = "UTF-8")
    xmlTreeParse(raw, useInternalNodes = TRUE)
  })
  getNodeSet(GEOLOC_DOMAINS, path)[[1]]
}


author_geolocation <- function(author, year) {
  loc <- geolocation_by_hand(author, year)

  if (is.null(loc) )
    loc <- geolocation_by_domain(author)

  addChildren(author, kids = list(loc))
}


scrap_authors_geolocation <- function(year) {
  file <- sprintf("../xml/isipta%s.xml", year)
  raw <- readLines(file, encoding = "UTF-8")
  xml <- xmlTreeParse(raw, useInternalNodes = TRUE)
  authors <- getNodeSet(xml, "//author")
  free(xml)

  dups <- duplicated(sapply(authors, function(x) xmlValue(x[[1]])))
  authors <- authors[!dups]

  res <- xmlAuthors(year)
  for ( author in authors )
    res$addNode(author_geolocation(author, year))

  res
}



### Scrap authors geolocation:

doit <- function(year) {
  i <- scrap_authors_geolocation(year)
  saveXML(i$value(), file = sprintf("../xml/geoloc_authors%s.xml", year))
}

#doit(1999)
#doit(2001)
#doit(2003)
#doit(2005)
#doit(2007)
#doit(2009)
#doit(2011)
doit(2013)


