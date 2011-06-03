
library("igraph")
library("RColorBrewer")

load("../data/coauthors.RData")

str(coauthors)
head(coauthors)



### Authors as nodes, papers as edges:

fromto <-
lapply(split(coauthors, coauthors$paperid),
       function(x) {
         if ( length(x[, "author"]) > 1 )
           y <- combn(x[, "author"], 2)
         else
           y <- rbind(x["author"], x["author"])

         data.frame(from = y[1, ],
                    to = y[2, ],
                    year = x[1, "year"],
                    paperid = x[1, "paperid"])
       })

fromto <- do.call(rbind, fromto)
fromto$color <- as.integer(fromto$year)

g <- graph.data.frame(fromto, directed = FALSE)

plot(g, vertex.size = 2, vertex.label = NA)



### Authors as edges, papers as nodes:

fromto <-
lapply(split(coauthors, coauthors$author),
       function(x) {
         ret <- NULL

         if ( length(x[, "paperid"]) > 1 ) {
           y <- combn(as.character(x[, "paperid"]), 2)
           ret <- data.frame(from = y[1, ],
                             to = y[2, ])
         }

         ret
       })

fromto <- do.call(rbind, fromto)


cols <- brewer.pal(6, "Set1")

tmp <- unique(coauthors[, 1:2])
vert <- data.frame(name = as.character(tmp$paperid),
                   color = cols[as.integer(tmp$year)],
                   stringsAsFactors = FALSE)

g <- graph.data.frame(fromto, directed = FALSE, vertices = vert)

pdf("papers.pdf", width = 5, height = 5)
par(mar = c(0, 0, 0, 0))
plot(g, vertex.size = 3, vertex.label = NA,
     layout = layout.fruchterman.reingold)
dev.off()

plot(g, vertex.size = 2, vertex.label = NA,
     layout = layout.graphopt)


