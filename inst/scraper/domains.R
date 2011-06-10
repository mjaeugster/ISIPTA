
library("stringr")

subdomain <- function(x, nlevel) {
  parts <- strsplit(x, "\\.")[[1]]
  parts <- tail(parts, nlevel)
  paste(parts, collapse = ".")
}


basedomain <- function(x) {
  nlevels <- nrow(str_locate_all(x, "\\.")[[1]])

  if ( nlevels == 1 )
    return(x)

  if ( grepl("\\.ac\\.", x) )
    return(subdomain(x, 3))

  if ( grepl("\\.com\\.", x) )
    return(subdomain(x, 3))

  subdomain(x, 2)
}


domain <- function(x) {
  x <- xmlValue(x)
  if ( grepl("@", x) )
    return(strsplit(x, "@")[[1]][[2]])

  NULL
}
