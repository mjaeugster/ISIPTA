
CACHE <- local({
  demos <- NULL

  list(get = function() demos,
       set = function(x) demos <<- x)
})


setup_demo_cache <- function() {
  envir <- new.env()
  file <- system.file("extdata", "demo-cache.RData", package = "ISIPTA")
  load(file, envir = envir)

  CACHE$set(envir$demos)
}



#' Find and summarize an ISIPTA author
#'
#' @param pattern Part of an author name
#'
#' @return \code{find_author} returns a \code{data.frame} with each
#'   author matching the \code{pattern} and the corresponding graph
#'   node id (see \code{demo("coauthors-network")}.
#'
#' @examples
#'   find_author("Gero")
#'
#' @export
find_author <- function(pattern) {
  if ( is.null(CACHE$get()) )
    setup_demo_cache()

  which <- grep(pattern, V(CACHE$get()$graph)$name)

  node <- as.character(which - 1)
  name <- V(CACHE$get()$graph)$name[which]

  data.frame(name, nodeid = node)
}



#' @param nodeid Node ID as shown in the graph (see
#'   \code{demo("coauthors-network")})
#'
#' @return \code{find_node} returns a \code{data.frame} with the
#'   corresponding author names.
#'
#' @examples
#'   find_node(81:83)
#'
#' @rdname find_author
#'
#' @export
find_node<- function(nodeid){
  if ( is.null(CACHE$get()) )
    setup_demo_cache()

   name<- V(CACHE$get()$graph)$name[nodeid + 1]
   data.frame(name = name, nodeid = nodeid)
}



#' @param name Exact author name as returned by \code{find_author}
#' @param show.papers Display paper details
#'
#' @return \code{summarize_author} returns a list with information on
#'   the author; \code{print.ISIPTA_author} nicely prints the result.
#'
#' @examples
#'   summarize_author("Gero Walter")
#'   summarize_author("Gero Walter", show.papers = TRUE)
#'
#' @rdname find_author
#'
#' @export
summarize_author <- function(name, show.papers = FALSE) {
  if ( is.null(CACHE$get()) )
    setup_demo_cache()

  stopifnot(name %in% CACHE$get()$authors_locations$author)

  ## Make R CMD check happy:
  author <- ncoauthors <- author1 <- author2 <- npapers <- NULL

  ret <- list(show.papers = show.papers)

  ## Author:
  ret$author <- find_author(name)

  ## Contributions:
  contribs <- subset(CACHE$papers_authors, author == name)$id

  ret$papers <-
    lapply(contribs, function(x) {
      y <- list()
      y$paper <- subset(CACHE$get()$papers, id == x)
      y$authors <- subset(CACHE$get()$papers_authors, id == x, select = author)
      y$authors <- do.call(rbind, lapply(y$authors$author, find_author))
      y
    })

  ret$conferences <-
    unlist(subset(CACHE$get()$conferences_contributors,
                  author == name, select = -author, drop = TRUE))

  ret$ncoauthors <-
    as.numeric(subset(CACHE$get()$papers_ncoauthors_overall,
                      author == name, select = ncoauthors))

  ret$ncoauthors_unique <-
    nrow(subset(CACHE$get()$coauthors_npairs, author1 == name | author2 == name))

  ret$npapers <-
    as.numeric(subset(CACHE$get()$authors_npapers_overall,
                      author == name, select = npapers))


  structure(ret, class = "ISIPTA_author")
}



#' @S3method print ISIPTA_author
print.ISIPTA_author <- function(x, show.papers = x$show.papers, ...) {

  pauthor <- function(x) {
    sprintf("%s (%s)", x["name"], x["nodeid"])
  }

  pnumbers <- function() {
    cbind(c("  Papers",
            "  Coauthors",
            "  Unique coauthors",
            "  Conferences"),
          c(x$npapers,
            x$ncoauthors,
            x$ncoauthors_unique,
            sum(x$conferences)))
  }

  names(x$conferences) <-
    sub("ISIPTA", "", names(x$conferences))

  cat(sprintf("ISIPTA summary for %s\n",
              pauthor(as.matrix(x$author)[1, ])), sep = "\n")

  cat("Number of ...:\n")
  cat(formatDL(pnumbers(), width = getOption("width")), sep = "\n")

  cat("\nContributions:\n")
  print(x$conferences == 1)

  if ( show.papers ) {
    cat("\nPapers:\n")
    for ( paper in x$papers ) {
      txt <- sprintf("%s. %s. %s.",
                     paper$paper$title,
                     paste(apply(paper$authors, 1, pauthor), collapse = ", "),
                     paper$paper$year)
      txt <- strwrap(txt, width = 0.7 * getOption("width"))
      txt <- paste(txt, collapse = "\n")
      cat(txt, "\n\n")
    }
  }
}
