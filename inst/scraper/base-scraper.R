
library("stringr")


### Data cleaning:

clean_title <- function(title) {
  title <- str_trim(title)
  title <- str_replace_all(title, "\\.+$", "")
  title
}


clean_keywords <- function(keywords) {
  n <- str_locate(keywords, "Keywords.")[1, "end"] + 1
  tmp <- str_sub(keywords, start = n)
  tmp <- str_trim(str_split(tmp, ",")[[1]])
  tmp <- str_replace_all(tmp, "\\.$", "")
  tmp <- tolower(tmp)

  tmp <- iconv(tmp, from = "UTF-8", to = "latin1")

  tmp
}


clean_abstract <- function(abstract) {
  abstract <- str_trim(abstract)
  abstract <- str_replace_all(abstract, "\\s+", " ")

  iconv(abstract, from = "UTF-8", to = "latin1")
}


clean_pdf <- function(pdf) {
  unname(pdf)
}


clean_email <- function(email) {
  if ( nrow(str_locate_all(email, "@")[[1]]) == 1 )
    str_replace_all(email, " ", "")
  else
    strsplit(email, " ")[[1]][1]
}


clean_author <- function(author) {
  normalize_author_name(author)
}


clean_authors <- function(authors, emails) {
  emails <- str_trim(do.call(rbind, emails))
  emails[, "name"] <- sapply(emails[, "name"], clean_author)
  emails[, "email"] <- sapply(emails[, "email"], clean_email)
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
  if ( isequal("Jacinto Martin", name) )
    return("Jacinto Martin")
  if ( isequal("Javier Hernandez", name) )
    return("Javier Hernandez")
  if ( isequal("Jose Pablo Arias", name) )
    return("Jose Pablo Arias")
  if ( isequal("Antonio Salmeron", name) )
    return("Antonio Salmeron")
  if ( isequal("Marcus Poggi de Aragao", name) )
    return("Marcus Poggi de Aragao")
  if ( isequal("Michele Vanmaele", name) )
    return("Michele Vanmaele")

  name
}

