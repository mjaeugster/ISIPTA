
library("XML")


xmlProceedings <- function(year, date, location) {
  doc <- xmlTree("proceedings")

  doc$addNode("year", year)
  doc$addNode(xmlConference(date, location))

  doc
}


xmlPaper <- function(id, title, keywords, abstract, pdf, authors) {
  addChildren(newXMLNode("paper"),
              kids = list(newXMLNode("id", id),
              newXMLNode("title", title),
              xmlPaperAuthors(authors),
              xmlPaperKeywords(keywords),
              newXMLNode("abstract", abstract),
              newXMLNode("pdf", pdf)))
}


xmlConference <- function(date, location) {
  n <- newXMLNode("conference")
  addChildren(n, kids = list(xmlProceedingsDate(date),
                 xmlLocation(location)))
}


xmlProceedingsDate <- function(date) {
  n <- newXMLNode("date")
  addChildren(n, kids = list(newXMLNode("start", date[1]),
                 newXMLNode("end", date[1])))
}


xmlPaperKeywords <- function(keywords) {
  addChildren(newXMLNode("keywords"),
              kids = lapply(keywords, function(x) newXMLNode("keyword", x)))
}


xmlPaperAuthors <- function(authors) {
  n <- newXMLNode("authors")

  for ( i in seq(length = nrow(authors)) ) {
    n0 <- newXMLNode("author")
    n0 <- addChildren(n0,
                      kids = list(newXMLNode("name", authors[i, "name"]),
                      newXMLNode("email", authors[i, "email"])))

    n <- addChildren(n, kids = list(n0))
  }

  n
}


xmlLocationCountry <- function(location) {
  n <- newXMLNode("country")
  addChildren(n, kids = list(newXMLNode("code", location["country_code"]),
                 newXMLNode("name", location["country_name"])))
}


xmlLocationCity <- function(location) {
  n <- newXMLNode("city")
  addChildren(n, kids = list(newXMLNode("name", location["city"]),
                 newXMLNode("latitude", location["city_lat"]),
                 newXMLNode("longitude", location["city_lon"])))
}

xmlLocationUniversity <- function(location) {
  n <- newXMLNode("university")
  addChildren(n, kids = list(newXMLNode("name", location["university"]),
                 newXMLNode("department", location["department"])))
}


xmlLocation <- function(location) {
  n <- newXMLNode("location")
  addChildren(n, kids = list(xmlLocationCountry(location),
                 xmlLocationCity(location),
                 xmlLocationUniversity(location)))

}


xmlDomain <- function(name, location) {
  n <- newXMLNode("domain")
  addChildren(n, kids = list(newXMLNode("name", name),
                 xmlLocation(location)))
}


xmlDomains <- function() {
  xmlTree("domains")
}


xmlAuthors <- function(year) {
  doc <- xmlTree("authors")
  doc$addNode("year", year)
  doc
}
