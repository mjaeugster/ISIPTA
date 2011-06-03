
library("XML")
library("stringr")



### Data cleaning: ###################################################

clean_title <- function(title) {
  title <- str_trim(title)
  title <- str_replace_all(title, "\\.+$", "")

}


clean_keywords <- function(keywords) {
  n <- str_locate(keywords, "Keywords.")[1, "end"] + 1
  tmp <- str_sub(keywords, start = n)
  tmp <- str_trim(str_split(tmp, ",")[[1]])
  tmp <- str_replace_all(tmp, "\\.$", "")
  tmp <- tolower(tmp)
  tmp
}


clean_abstract <- function(abstract) {
  abstract <- str_trim(abstract)
  str_replace_all(abstract, "\\s+", " ")
}


clean_pdf <- function(pdf) {
  unname(pdf)
}


clean_authors <- function(authors, emails) {
  emails <- str_trim(do.call(rbind, emails))
  emails[, "name"] <- sapply(emails[, "name"],
                             normalize_author_name)
  emails
}



### Author names:

normalize_author_name <- function(name) {
  isequal<- function(ref_name, name, dist = 0.3) {
    length(agrep(ref_name, name, max.distance = dist)) == 1
  }

  if ( isequal("Serafin Moral", name) )
    return("Serafin Moral")
  if ( isequal("Joaquin Abellan", name, 0.4) )
    return("Joaquin Abellan")
  if ( isequal("Damjan Skulj", name) )
    return("Damjan Skulj")
  if ( isequal("Peter Harremoes", name) )
    return("Peter Harremoes")
  if ( isequal("Frederic Pichon", name) )
    return("Frederic Pichon")
  if ( isequal("Christofer Waldenstrom", name) )
    return("Christofer Waldenstrom")
  if ( isequal("Andres Cano", name) )
    return("Andres Cano")
  if ( isequal("Manuel Gomez", name) )
    return("Manuel Gomez")
  if ( isequal("Volker Kraetschmer", name) )
    return("Volker Kraetschmer")
  if ( isequal("Cedric Baudrit", name) )
    return("Cedric Baudrit")
  if ( isequal("Raphael Giraud", name) )
    return("Raphael Giraud")
  if ( isequal("Vladislav Bina", name) )
    return("Vladislav Bina")
  if ( isequal("Sebastian Maass", name) )
    return("Sebastian Maass")
  if ( isequal("Michele Cohen", name) )
    return("Michele Cohen")
  if ( isequal("Ines Couso", name) )
    return("Ines Couso")
  if ( isequal("Jioina Vejnarova", name) )
    return("Jioina Vejnarova")


  name
}





### XML data base: ###################################################

xmlProceedings <- function(year, date, location) {
  doc <- xmlTree("proceedings")

  doc$addNode("year", year)
  doc$addNode(xmlConference(date, location))

  doc
}


xmlPaper <- function(id, title, keywords, abstract, pdf, authors) {
  addChildren(newXMLNode("paper"),
              kids = list(
              newXMLNode("id", id),
              newXMLNode("title", title),
              xmlPaperAuthors(authors),
              xmlPaperKeywords(keywords),
              newXMLNode("abstract", abstract),
              newXMLNode("pdf", pdf)))
}


xmlConference <- function(date, location) {
  n <- newXMLNode("conference")
  addChildren(n, kids = list(xmlProceedingsDate(date),
                 xmlProceedingsLocation(location)))
}


xmlProceedingsDate <- function(date) {
  n <- newXMLNode("date")
  addChildren(n, kids = list(newXMLNode("start", date[1]),
                 newXMLNode("end", date[1])))
}


xmlProceedingsLocation <- function(location) {
  n <- newXMLNode("location")
  addChildren(n, kids = list(newXMLNode("country", location[1]),
                 newXMLNode("city", location[2]),
                 newXMLNode("university", location[3]),
                 newXMLNode("department", location[4])))
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
