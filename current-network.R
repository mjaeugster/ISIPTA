

library("ISIPTA")

demo("coauthors-network", package = "ISIPTA",
     verbose = FALSE, echo = FALSE, ask = FALSE)



png("graph.png", width = 600, height = 600, pointsize = 10)
par(mar = c(0, 0, 0, 0))
set.seed(1234)
plot(graph,
     vertex.size = 5,
     vertex.color = "gray90",
     vertex.frame.color = "gray90",
     vertex.label.color = "black",
     edge.color = "SkyBlue2",
     layout = layout.fruchterman.reingold)
dev.off()


find_author("Gero")
summarize_author("Gero Walter", show.papers = TRUE)
