
load_demo <- function(demo, envir) {
  sys.source(demo, envir = envir)
}

load_demos <- function() {
  # when the package is packed
  demos <- list.files(system.file("demo", package = "ISIPTA"),
                      pattern = ".R", full.names = TRUE)
  # when package is not ready
  # (one needs to comment out all library("ISIPTA"), data(..., library="ISIPTA") statements in the demos)
  #demos <- list.files("./../../demo",
  #                    pattern = ".R", full.names = TRUE)
  
  op <- options(warn = -1)
  envir <- new.env()
  for ( demo in demos )
    load_demo(demo, envir)
  options(op)

  # when the package is packed
  data("papers", package = "ISIPTA", envir = envir)
  data("papers_authors", package = "ISIPTA", envir = envir)
  data("authors_locations", package = "ISIPTA", envir = envir)
  # when package is not ready
  #load("./../../data/papers.RData", envir = envir)
  #load("./../../data/papers_authors.RData", envir = envir)
  #load("./../../data/authors_locations.RData", envir = envir)
  #load("./../../data/conferences.RData", envir = envir)
  #load("./../../data/papers_keywords.RData", envir = envir)
  
  envir
}

demos <- load_demos()
save(demos, file = "demo-cache.RData")


