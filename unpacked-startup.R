# for starting up the ISIPTA not-yet-package
# from within the 'package' folder "ISIPTA"

#install.packages(c("igraph", "reshape2", "colorspace", "geosphere", "rworldmap", "ggplot2", "plyr", "stringr"))
library(igraph)
library(reshape2)
library(colorspace)
library(geosphere)
library(rworldmap)
library(ggplot2)
library(plyr)
library(stringr)

load("./data/authors_locations.RData")
load("./data/conferences.RData")
load("./data/papers.RData")
load("./data/papers_authors.RData")
load("./data/papers_keywords.RData")
 
setwd("./demo")                   # depends on...
source("simple-summary.R")        #
source("authors-locations.R")     #
source("country-contributions.R") # authors-locations.R
source("authors-per-paper.R")     #
source("regular-contributors.R")  # simple-summary.R
source("papers-per-author.R")     #
source("coauthors-per-author.R")  # authors-per-paper.R
source("coauthors-network.R")     # regular-contributors.R
source("coauthors-worldmap.R")    # coauthors-network.R
source("coauthors-network2013.R") # coauthors-network.R, produces / overwrites pdfs!!!

setwd("./../R")
source("graph.R")
setwd("./..")

# now all should be ready!






#