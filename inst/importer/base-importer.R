
library("stringr")


### Data cleaning:

clean_title <- function(title) {
  title <- str_trim(title)
  title <- str_replace_all(title, "\\.+$", "")
  title
}


clean_keywords <- function(keywords) {
  keywords
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
  str_trim(email)
}


clean_author <- function(author) {
  author <- str_trim(author)
  #iconv(author, from = "latin1", to = "ASCII")
  author
}


clean_authors <- function(authors, emails) {
  authors <- sapply(authors, clean_author)
  emails <- sapply(emails, clean_author)

  cbind(name = authors, email = emails)
}

