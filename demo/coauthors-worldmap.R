### Geographic visualization of the co-authoring -- the nodes
### represents authors, two authors are connected if they are
### co-authors within a conference.

library("colorspace")
library("geosphere")
library("rworldmap")

data("papers_authors")
data("authors_locations")



### Location data: ###################################################

### Edgelist of co-authors within a year:
el <- as.edgelist(papers_authors,
                  edge.var = "id",
                  node.var = "author")


## Add year of co-authoring:
el <- merge(el, subset(papers_authors, select = -c(author)))


## Add authors' locations in the corresponding year:
el <- merge(el, authors_locations,
            by.x = c("from", "year"),
            by.y = c("author", "year"))

el <- merge(el, authors_locations,
            by.x = c("to", "year"),
            by.y = c("author", "year"),
            suffixes = c(".from", ".to"))



### Visualization: ###################################################

add_lines <- function(x, ...) {
  for ( i in seq(length = nrow(x)) ) {
    p1 <- c(x = x$city_lon.from[i], y = x$city_lat.from[i])
    p2 <- c(x = x$city_lon.to[i], y = x$city_lat.to[i])

    if ( any(is.na(c(p1, p2))) )
      next

    p <- gcIntermediate(p1, p2, n = 100, breakAtDateLine = TRUE,
                        addStartEnd = TRUE)

    if ( is.list(p) )
      lapply(p, lines, ...)
    else
      lines(p, ...)
  }
}


years <- sort(as.character(unique(el$year)))
cols <- rainbow_hcl(length(years))
names(cols) <- years


par(mfrow = c(3, 2), mar = c(0, 0, 0, 0))
for ( y in years ) {
  dat <- subset(el, year == y)

  plot(getMap(), border = "gray")
  add_lines(dat, col = cols[y])
  mtext(sprintf("year = %s", y), 3, -2, cex = 0.7)
}

