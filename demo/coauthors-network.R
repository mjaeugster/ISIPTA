### Coauthor network -- the nodes represent papers with an edge
### between two papers if they share at least one author; the node
### color indicates the conference year.

library("colorspace")
library("igraph")

data("papers_authors")



### Network: #########################################################

## Edges:
el <- as.edgelist(papers_authors,
                  edge.var = "author",
                  node.var = "id")


## Nodes:
years <- unique(papers_authors$year)

cols <- rainbow_hcl(length(years))
names(cols) <- as.character(years)

no <- data.frame(name = as.character(papers_authors$id),
                 color = cols[as.character(papers_authors$year)],
                 stringsAsFactors = FALSE)
no <- unique(no)


## Graph:
gr <- graph.data.frame(el, directed = FALSE, vertices = no)

gr
summary(gr)



### Analysis: ########################################################


### Visualization:
plot(gr, vertex.size = 3, vertex.label = NA,
     layout = layout.fruchterman.reingold)
legend("topleft", legend = years, pch = 19, col = cols)


