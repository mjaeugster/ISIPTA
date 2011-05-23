
library("XML")
library("stringr")

publication <- function(title = NA_character_, keywords = NA_character_,
                        abstract = NA_character_, pdf = NA_character_,
                        author = NA_character_, address = NA_character_,
                        email = NA_character_) {
  
  structure(list(title = title,
                 author = author(name, address, email),
                 keywords = keywords,
                 abstract = abstract,
                 pdf = pdf),
            class = c("publication", "list"))
}


author <- function(name, address, email) {
  n <- c(length(name), length(address), length(email))
  stopifnot(length(unique(n)) == 1)
  n <- n[1]

  create.author <- function(n, a, e) {
    structure(list(name = n,
                   address = a,
                   email = e),
              class = c("author", "list"))
  }
  
  structure(Map(create.author, name, address, email),
            class = c("authors", "list"))
}
