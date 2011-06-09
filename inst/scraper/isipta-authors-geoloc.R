### Estimate the geolocation of the authors.

source("xml.R")
source("domains.R")


GEOLOC_DOMAINS <- xmlTreeParse(readLines("../xml/geoloc_domains.xml",
                                         encoding = "UTF-8"),
                               useInternalNodes = TRUE)


geolocation_by_hand <- function(author, year) {
  NULL
}


geolocation_by_domain <- function(author) {
  d <- domain(author[[2]])

  if ( is.null(d) )
    return(NULL)

  d <- basedomain(d)

  path <- sprintf("//domain[name='%s']/location", d)
  getNodeSet(GEOLOC_DOMAINS, path)[[1]]
}


author_geolocation <- function(author, year) {
  loc <- geolocation_by_hand(author, year)

  if (is.null(loc) )
    loc <- geolocation_by_domain(author)

  addChildren(author, kids = list(loc))
}


isipta_authors_geolocation <- function(year) {
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


  saveXML(res$value(), file = sprintf("../xml/geoloc_authors%s.xml", year))
}


isipta_authors_geolocation(1999)
isipta_authors_geolocation(2001)
isipta_authors_geolocation(2005)
isipta_authors_geolocation(2007)
isipta_authors_geolocation(2009)


