### first run coauthors-network.R!

networkseed <- 1234
networkseed <- 2015
### big graph 2013 with rownames(vertices), i.e. rownumber as id
set.seed(networkseed)
pdf("../../../network2013-1.pdf", width=20, height=20)
op1 <- par(mar = c(1, 0, 0, 0))
plot(graph,
     vertex.size = 3.5,
     vertex.color = "gray90",
     vertex.label = rownames(vertices),
     vertex.frame.color = "gray90",
     vertex.label.color = "black",
     edge.color = "SkyBlue2",
     layout = layout.fruchterman.reingold)
legend("topleft",
       legend = sort(unique(edgelist$width)),
       lwd = sort(unique(edgelist$width)),
       col = "SkyBlue2",
       bty = "n")
par(op1)
dev.off()

#namestest <- (as.character(authors_years$author) == as.character(vertices[,1]))

### Graphs over time:
years <- levels(coauthors_pairs$year)
years <- sapply(years, grep,
                colnames(coauthors_years), value = TRUE)
#years <- years[-length(years)] # to exclude latest graph

pdf("../../../networkevolution-1.pdf", width=16, height=8)
#op <- par(mfrow = c(1, length(years)))
op <- par(mfrow = c(2, length(years)/2)) # for 8 conferences
for ( i in years ) {
  ewidth <- coauthors_years[[i]]
  ecolor <- ifelse(coauthors_years[[i]] > 0, "SkyBlue2", NA)
  vcolor <- ifelse(authors_years[[i]] > 0, "black", NA)
  fcolor <- ifelse(authors_years[[i]] > 0, "black", NA)
  
  op1 <- par(mar = c(1, 0, 0, 0))
  set.seed(networkseed)
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
dev.off()

#