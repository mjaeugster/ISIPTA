
library("colorspace")
library("maps")
library("geosphere")
library("rworldmap")


load("../data/coauthors.RData")
load("../data/locations.RData")

str(coauthors)
str(locations)

coauthor_location <- function(y) {
  co1 <- subset(coauthors, year == y)
  lo1 <- subset(locations, year == y)

  y <- merge(co1, lo1)
  y$year <- y$year[, drop = TRUE]
  y$paperid <- y$paperid[, drop = TRUE]
  y
}


gclines <- function(x, ...) {
  if ( any(is.na(x)) )
    return(NULL)

  p <- gcIntermediate(x[1, ], x[2, ], n = 100, breakAtDateLine = TRUE)
  if ( is.list(p) )
    lapply(p, lines, ...)
  else
    lines(p, ...)
}


map_add_connections <- function(year, col) {
  x <- coauthor_location(year)

  fromto <-
      lapply(split(x, x$paperid),
             function(x) {
               if ( length(x[, "author"]) > 1 )
                   y <- combn(x[, "author"], 2)
               else
                   y <- rbind(x["author"], x["author"])

               fcoord <- x[match(y[1,], x$author),
                           c("latitude", "longitude")]
               tcoord <- x[match(y[2,], x$author),
                           c("latitude", "longitude")]

               z <- data.frame(from = y[1, ],
                               fromcoord = fcoord,
                               to = y[2, ],
                               tocoord = tcoord,
                               year = x[1, "year"],
                               paperid = x[1, "paperid"])

               z
           })

  points(x$longitude, x$latitude, col = "black", pch = 19, cex = 0.5)

  for ( p in fromto ) {
      for ( i in 1:nrow(p) ) {
          p1 <- structure(p[i, c("fromcoord.longitude", "fromcoord.latitude")],
                          names = c("x", "y"))
          p2 <- structure(p[i, c("tocoord.longitude", "tocoord.latitude")],
                          names = c("x", "y"))
          gclines(rbind(p1, p2), col = col)
      }
  }

  invisible(fromto)
}


years <- levels(coauthors$year)
cols <- rainbow_hcl(length(years))

pdf(file = "map.pdf", width = 10, height = 10)
par(mfrow = c(3, 2))
for ( i in seq(along = years) ) {
  par(mar = c(0, 0, 0, 0))
  plot(getMap(), border = "gray")
  mtext(sprintf("year = %s", years[i]), 3, -2, cex = 0.7)
  map_add_connections(years[i], cols[i])
}
dev.off()


