
setup_demo_cache <- function() {
  envir <- new.env()
  file <- system.file("extdata", "demo-cache.RData", package = "ISIPTA")
  load(file, envir = envir)
  envir$demos
}

CACHE <- setup_demo_cache()


#' Find authors and node ids for a given pattern.
find_author <- function(pattern) {
  which <- grep(pattern, V(CACHE$graph)$name)

  node <- as.character(which - 1)
  name <- V(CACHE$graph)$name[which]

  data.frame(name, nodeid = node)
}


find_node<- function(nodeid){
   name<- V(CACHE$graph)$name[nodeid + 1]
   data.frame(name = name, nodeid = nodeid)
}


summarize_author <- function(name, show.papers = FALSE) {
  stopifnot(name %in% CACHE$authors_locations$author)

  ret <- list(show.papers = show.papers)

  ## Author:
  ret$author <- find_author(name)

  ## Contributions:
  contribs <- subset(CACHE$papers_authors, author == name)$id

  ret$papers <-
    lapply(contribs, function(x) {
      y <- list()
      y$paper <- subset(CACHE$papers, id == x)
      y$authors <- subset(CACHE$papers_authors, id == x, select = author)
      y$authors <- do.call(rbind, lapply(y$authors$author, find_author))
      y
    })

  ret$conferences <-
    unlist(subset(CACHE$conferences_contributors,
                  author == name, select = -author, drop = TRUE))

  ret$ncoauthors <-
    as.numeric(subset(CACHE$papers_ncoauthors_overall,
                      author == name, select = ncoauthors))

  ret$ncoauthors_unique <-
    nrow(subset(CACHE$coauthors_npairs, author1 == name | author2 == name))

  ret$npapers <-
    as.numeric(subset(CACHE$authors_npapers_overall,
                      author == name, select = npapers))


  structure(ret, class = "ISIPTA_author")
}


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
