### Number of contributions by year and country.

#library("ISIPTA")
library("rworldmap")

#demo("authors-locations", package = "ISIPTA",
#     verbose = FALSE, echo = FALSE, ask = FALSE)

#data("papers_authors", package = "ISIPTA")
#data("authors_locations", package = "ISIPTA")


papers_authors_locations <- merge(papers_authors,
                                  authors_locations, all = TRUE)


## Countries per paper:
papers_countries <-
  ddply(papers_authors_locations, .(id),
        function(x) {
          ac <- t(as.matrix(table(x$country_code)))

          data.frame(year = x$year[1],
                     id = x$id[1],
                     nauthors = nrow(x),
                     as.data.frame(ac))
        })


## Country contributions per paper:
papers_country_contributions <-
  cbind(papers_countries[, 1:3],
        papers_countries[, -(1:3)] / papers_countries[, 3])


## Country contributions per year:
t3 <- daply(papers_country_contributions, .(year),
            function(x) {
              colSums(x[, -(1:3)])
            })

t3



### Visualization by region and by year: #############################

data("countryRegions", package = "rworldmap")

t3melt <- melt(t3, varnames = c("year", "country_code"))
# countryRegions now has only ISO3 country codes,
# so we must convert the two-letter ISO2 to the three-letter ISO3 code
t3melt$country_code3 <- NA
# isoToName() does not like character vectors.
# this loop throws 8 warnings but the result still seems fine (?)
for (i in 1:dim(t3melt)[1]){
  t3melt$country_code3[i] <- isoToName(iso=as.character(t3melt$country_code[i]), nameColumn='ISO_A3')
}
t3melt$region <- countryRegions[match(t3melt$country_code3,
                                      countryRegions$ISO3), "GEO3major"]
t3melt$region <- t3melt$region[, drop = TRUE]
t3melt$year <- ordered(t3melt$year)

t3melt <- ddply(t3melt, .(year, region), numcolwise(sum))


ggplot(t3melt, aes(year, value, group = region, colour = region)) +
  geom_point() + geom_line()



### Visualization of authors locations versus country contributions: #

t23melt <- rbind(cbind(t2melt, what = "Unique authors"),
                 cbind(t3melt, what = "Contributions"))
t23melt$what <- factor(t23melt$what, levels = c("Unique authors", "Contributions"))

ggplot(t23melt, aes(year, value, group = region, colour = region)) +
  geom_point() + geom_line() +
  facet_grid(. ~ what)
