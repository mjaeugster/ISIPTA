### Coauthors network; i.e., authors as vertices and two vertices
### joined by an edge if the two authors have written a joint paper.

library("ISIPTA")

demo("regular-contributors", package = "ISIPTA",
     verbose = FALSE, echo = FALSE, ask = FALSE)

data("papers_authors", package = "ISIPTA")


coauthors_pairs <- ddply(papers_authors, .(id),
                         function(x) {
                           if ( nrow(x) > 1 ) {
                             authors <- sort(as.character(x$author))
                             pairs <- combn(authors, 2)

                             data.frame(author1 =
                                        factor(pairs[1, ],
                                               levels = levels(x$author)),

                                        author2 =
                                        factor(pairs[2, ],
                                               levels = levels(x$author)),

                                        year = x$year[1],
                                        id = x$id[1])
                           }
                         })

coauthors_pairs <- within(coauthors_pairs, {
  year <- ordered(year)
  id <- factor(id)
})



## Reduce to the number of each pair:
coauthors_npairs <- ddply(coauthors_pairs, .(author1, author2),
                          function(x) {
                            c(npairs = nrow(x))
                          })



### Overall collaboration graph: #####################################

## Edgelist; width of the edge is the number of joint papers:
edgelist <- within(coauthors_npairs, {
  width <- npairs
  npairs <- NULL
})



## Vertices:
vertices <- data.frame(name = levels(edgelist$author1))



## Graph:
graph <- graph.data.frame(edgelist,
                          directed = FALSE,
                          vertices = vertices)

summary(graph)



### Visualization of the graph: ######################################

set.seed(1234)
plot(graph,
     vertex.size = 5,
     vertex.color = "gray90",
     vertex.frame.color = "gray90",
     vertex.label.color = "black",
     edge.color = "SkyBlue2",
     layout = layout.fruchterman.reingold)

legend("topleft",
       legend = sort(unique(edgelist$width)),
       lwd = sort(unique(edgelist$width)),
       col = "SkyBlue2",
       bty = "n")



### Average path length, i.e., the deegres of separation: ############

average.path.length(graph)



### The longest shortest path, i.e., the diameter: ###################

diameter(graph)

V(graph)[get.diameter(graph)]



### Distance distributions: ##########################################

distances <- shortest.paths(graph)
dimnames(distances) <- list(V(graph)$name, V(graph)$name)



### Personal distributions of the "regular contributors":

regulars <- subset(authors_ncontributions,
                   ncontribs == nconferences)$author

regulars_distances <-
  distances[, match(regulars, colnames(distances)), drop = FALSE]

ggplot(melt(regulars_distances), aes(value)) +
  geom_density(aes(y = ..count..), fill = "SkyBlue2") +
  facet_grid(X2 ~ .)



### Evolution of the network over time: ##############################

## Vertices, i.e., coauthors, by years:
coauthors_years <- ddply(coauthors_pairs, .(author1, author2),
                         function(x) {
                           as.data.frame(t(as.matrix(table(x$year))))
                         })

colnames(coauthors_years) <- c("author1", "author2",
                               sprintf("ISIPTA%s",
                                       levels(coauthors_pairs$year)))

coauthors_years <- cbind(coauthors_years[, 1:2],
                         t(apply(coauthors_years[, -c(1:2)], 1, cumsum)))


## Edges, i.e., authors, by years:
authors_years <- cbind(conferences_contributors[, 1, drop = FALSE],
                       t(apply(conferences_contributors[, -c(1)], 1, cumsum)))


### Graphs over time:

years <- levels(coauthors_pairs$year)
years <- sapply(years, grep,
                colnames(coauthors_years), value = TRUE)

op <- par(mfrow = c(1, length(years)))
for ( i in years ) {

  ewidth <- coauthors_years[[i]]
  ecolor <- ifelse(coauthors_years[[i]] > 0, "SkyBlue2", "white")
  vcolor <- ifelse(authors_years[[i]] > 0, "black", "white")
  fcolor <- ifelse(authors_years[[i]] > 0, "black", "white")

  op1 <- par(mar = c(1, 0, 0, 0))
  set.seed(1234)
  plot(graph,
       vertex.size = 3,
       vertex.label = NA,
       vertex.color = vcolor,
       vertex.frame.color = fcolor,
       edge.color = ecolor,
       edge.width = ewidth,
       layout = layout.fruchterman.reingold)

  mtext(i, side = 1, line = 0)
  par(op1)
}
par(op)
