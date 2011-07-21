### Coauthors network on the world map.

library("ISIPTA")
source("coauthors-network.R")

data("authors_locations", package = "ISIPTA")


## Extend with geolocations:
coauthors_pairs <- merge(coauthors_pairs, authors_locations,
                         by.x = c("author2", "year"),
                         by.y = c("author", "year"),
                         sort = FALSE)

coauthors_pairs <- merge(coauthors_pairs, authors_locations,
                         by.x = c("author1", "year"),
                         by.y = c("author", "year"),
                         suffixes = c(".author2", ".author1"),
                         sort = FALSE)



### Visualization of the world map: ##################################

a <- subset(coauthors_pairs, year == 2007)

library("rworldmap")
library("rgdal")


plot(getMap())
for ( i in seq(length = nrow(a)) ) {
p1 <- c(x = a[i, "city_lon.author1"], y = a[i, "city_lat.author1"])
p2 <- c(x = a[i, "city_lon.author2"], y = a[i, "city_lat.author2"])

l <- gcIntermediate(p1, p2, n = 100,
                    breakAtDateLine = TRUE,
                    addStartEnd = TRUE)

if ( is.list(l) )
  lapply(l, lines, col = 2, lwd = 1)
else
  lines(l, col = 2, lwd = 1)
}

