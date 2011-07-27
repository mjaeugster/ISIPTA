
load_demo <- function(demo, envir) {
  sys.source(demo, envir = envir)
}

load_demos <- function() {
  demos <- list.files(system.file("demo", package = "ISIPTA"),
                      pattern = ".R", full.names = TRUE)

  op <- options(warn = -1)
  envir <- new.env()
  for ( demo in demos )
    load_demo(demo, envir)
  options(op)

  data("papers", package = "ISIPTA", envir = envir)
  data("papers_authors", package = "ISIPTA", envir = envir)
  data("authors_locations", package = "ISIPTA", envir = envir)

  envir
}

demos <- load_demos()
save(demos, file = "demo-cache.RData")


