
library("ISIPTA")

library("ggplot2")
library("plyr")
library("rworldmap")
library("geosphere")
library("colorspace")


data("papers", package = "ISIPTA")
data("papers_authors", package = "ISIPTA")
data("authors_locations", package = "ISIPTA")
data("conferences", package = "ISIPTA")

papers_authors_locations <- merge(papers_authors, authors_locations, all = TRUE)

str(papers)
str(papers_authors)
str(authors_locations)
str(papers_authors_locations)
str(conferences)



### Simple numbers by years: #########################################

t1 <- data.frame(year = sort(unique(papers$year)))

## Number of papers:
t1$papers <- as.numeric(table(papers$year))


## Number of paper authors:
t1$paper_authors <- as.numeric(table(papers_authors$year))


## Number of unique authors:
t1$unique_authors <-
    as.numeric(daply(papers_authors, .(year),
                     function(x) {
                       nlevels(x$author[, drop = TRUE])
                     }))


### Table:
t1

### Visualization:
t1melt <- melt(t1, id = "year")
t1melt$year <- ordered(t1melt$year)

ggplot(t1melt, aes(year, value, group = variable, colour = variable)) +
    geom_point() + geom_line()

##- Minimum number of papers 2003, but not minimum numbers of
##- paper_authors and unique_authors --> see authors per paper
##- visualization.



### Number of unqiue authors by year and country: ####################

t2 <- table(authors_locations$year, authors_locations$country_code)


### Table:
t2


### Map absolute numbers:
t2absolut <- data.frame(country_code = colnames(t2),
                        value = colSums(t2))

t2map <- joinCountryData2Map(t2absolut, joinCode = "ISO2",
                             nameJoinColumn = "country_code")

values <- seq(min(t2absolut$value), max(t2absolut$value))
pal <- rev(sequential_hcl(length(values), power = 1.2))


## World:
mapCountryData(t2map, nameColumnToPlot = "value",
               colourPalette = pal,
               catMethod = values,
               mapTitle = "",
               addLegend = TRUE,
               oceanCol = gray(0.95),
               missingCountryCol = "white")

## Europe:
mapCountryData(t2map, nameColumnToPlot = "value",
               mapRegion = "europe",
               colourPalette = pal,
               catMethod = values,
               mapTitle = "",
               addLegend = TRUE,
               oceanCol = gray(0.95),
               missingCountryCol = "white")


# plot(t2map[t2map@data$ISO2 %in% c("US", "DE"), ])



### Visualize by region and by year:
data("countryRegions", package = "rworldmap")

t2melt <- melt(t2, varnames = c("year", "country_code"))
t2melt$region <- countryRegions[match(t2melt$country_code,
                                      countryRegions$ISO2), "GEO3major"]
t2melt$region <- t2melt$region[, drop = TRUE]
t2melt$year <- ordered(t2melt$year)

t2melt <- ddply(t2melt, .(year, region), numcolwise(sum))

ggplot(t2melt, aes(year, value, group = region, colour = region)) +
  geom_point() + geom_line()




### Number of paper contributions by year and country: ###############

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

str(papers_countries)
head(papers_countries)


## Country contributions per paper:
contributions_countries <-
  cbind(papers_countries[, 1:3],
        papers_countries[, -(1:3)] / papers_countries[, 3])

head(contributions_countries)


## Country contributions per year:
t3 <- daply(contributions_countries, .(year),
            function(x) {
              colSums(x[, -(1:3)])
            })


### Table:
t3


### Visualize by region and by year:
t3melt <- melt(t3, varnames = c("year", "country_code"))
t3melt$region <- countryRegions[match(t3melt$country_code,
                                      countryRegions$ISO2), "GEO3major"]
t3melt$region <- t3melt$region[, drop = TRUE]
t3melt$year <- ordered(t3melt$year)

t3melt <- ddply(t3melt, .(year, region), numcolwise(sum))

ggplot(t3melt, aes(year, value, group = region, colour = region)) +
  geom_point() + geom_line()



### Authors per paper, i.e., collaborating authors: ##################

papers_nauthors <-
  ddply(papers_authors, .(id),
        function(x) {
          data.frame(year = x$year[1],
                     id = x$id[1],
                     nauthors = nrow(x))
        })


t4 <- table(papers_nauthors$year, papers_nauthors$nauthors)



### Table:
t4


### Visualizing the frequencies:

## ... absolut numbers of authors per paper:
ggplot(papers_nauthors, aes(factor(nauthors))) + geom_bar()

## ... numbers of authors per paper per year:
ggplot(papers_nauthors, aes(factor(nauthors), fill = factor(year))) + geom_bar()


##- See network; a lot of unconnected nodes ;)



### Visualizing the trend:
t4melt <- melt(t4, varnames = c("year", "nauthors"))


## ... grouped by year:
ggplot(t4melt, aes(factor(nauthors), value, group = factor(year), colour = factor(year))) +
    geom_point() + geom_line()


## ... grouped by number of authors:
ggplot(t4melt, aes(factor(year), value, group = factor(nauthors), colour = factor(nauthors))) +
    geom_point() + geom_line()



### Papers per author, i.e., productivity ############################
### Coauthors per author, i.e., collaboration ########################



### Regular contributors: ############################################

authors_locations$year <- ordered(authors_locations$year)

conferences_contributors <-
  ddply(authors_locations, .(author),
        function(x) {
            data.frame(author = x$author[1],
                       t(as.matrix(table(x$year))))
        })

colnames(conferences_contributors) <- c("author",
                                        sprintf("ISIPTA%s",
                                                levels(authors_locations$year)))

str(conferences_contributors)
head(conferences_contributors)


authors_ncontributions <- data.frame(author = conferences_contributors$author,
                                     ncontribs = rowSums(conferences_contributors[, -1]))


str(authors_ncontributions)
head(authors_ncontributions)



### Contribution "distribution":
t5 <- table(authors_ncontributions$ncontribs)

t5

ggplot(melt(t5, varnames = c("ncontribs")), aes(ordered(ncontribs), value)) +
  geom_bar()


### The "regular contributors":
nconferences <- nlevels(authors_locations$year)

subset(authors_ncontributions, ncontribs == nconferences)


### Contributors flow:
flow <- 2:nconferences
names(flow) <- paste(colnames(conferences_contributors[, -c(1, 6)]),
                     colnames(conferences_contributors[, -c(1, 2)]), sep = "-")

contributors_flow <- sapply(flow,
                            function(i) {
                              i <- i + 1
                              as.logical(conferences_contributors[, i-1]) &
                              as.logical(conferences_contributors[, i])
                            })


## Number of authors contributing again the following conference:
colSums(contributors_flow)


## ... in relation to the maximum number of contributors to lose:
max_loss <- sapply(flow,
                   function(i) {
                     min(t1$unique_authors[i-1], t1$unique_authors[i])
                   })

colSums(contributors_flow) / max_loss



### Coauthors network: ###############################################

coauthor_pairs <- ddply(papers_authors, .(id),
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

head(coauthor_pairs)
str(coauthor_pairs)

coauthor_npairs <- ddply(coauthor_pairs, .(author1, author2),
                         function(x) {
                           c(x$author[1], x$author[2], npapers = nrow(x))
                         })

head(coauthor_npairs)
str(coauthor_npairs)



### Network:
coauthor_npairs$width <- coauthor_npairs$npapers

gr1 <- graph.data.frame(coauthor_npairs,
                        directed = FALSE,
                        vertices = data.frame(name = levels(coauthor_npairs$author1)))


## Plot:
par(mar = c(0, 0, 0, 0))
set.seed(1234)
plot(gr1, vertex.size = 3, vertex.label = NA, vertex.color = "lightgray",
     edge.color = "SkyBlue2",
     layout = layout.fruchterman.reingold)

legend("topleft", legend = sort(unique(coauthor_npairs$npapers)),
       col = "SkyBlue2", lwd = sort(unique(coauthor_npairs$npapers)))



### Location based coauthors network: ################################

coauthors_distances <- merge(coauthor_pairs,
                             authors_locations,
                             by.x = c("author2", "year"),
                             by.y = c("author", "year"),
                             sort = FALSE)

coauthors_distances <- merge(coauthors_distances,
                             authors_locations,
                             by.x = c("author1", "year"),
                             by.y = c("author", "year"),
                             suffixes = c(".author2", ".author1"))


str(coauthors_distances)
head(coauthors_distances)



### Country- and region-based coauthoring:
t6 <- table(coauthors_distances$country_code.author1,
            coauthors_distances$country_code.author2)

t6melt <- melt(t6, varnames = c("country_code1", "country_code2"))

ggplot(t6melt, aes(country_code1, country_code2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue")


## Reduce to world regions:
t6melt$region1 <- countryRegions[match(t6melt$country_code1,
                                       countryRegions$ISO2), "GEO3major"]
t6melt$region1 <- t6melt$region1[, drop = TRUE]

t6melt$region2 <- countryRegions[match(t6melt$country_code2,
                                       countryRegions$ISO2), "GEO3major"]
t6melt$region2 <- t6melt$region2[, drop = TRUE]

t6melt <- ddply(t6melt, .(region1, region2), numcolwise(sum))


ggplot(t6melt, aes(region1, region2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_continuous()



### Network:
coauthors_distances$distance <-
  distVincentyEllipsoid(coauthors_distances[, c("city_lon.author1", "city_lat.author1")],
                        coauthors_distances[, c("city_lon.author2", "city_lat.author2")])


coauthors_npairs <- ddply(coauthors_distances, .(author1, author2),
                         function(x) {
                           c(x$author[1], x$author[2], npapers = nrow(x),
                             distance = mean(x$distance))
                         })

coauthor_npairs$width <- coauthor_npairs$npapers

gr1 <- graph.data.frame(coauthor_npairs,
                        directed = FALSE,
                        vertices = data.frame(name = levels(coauthor_npairs$author1)))


## Plot:

l <- layout.fruchterman.reingold(gr1, weights = coauthors_npairs$distance)

par(mar = c(0, 0, 0, 0))
set.seed(1234)
plot(gr1, vertex.size = 3, vertex.label = NA, vertex.color = "lightgray",
     edge.color = "SkyBlue2",
     layout = l )

legend("topleft", legend = sort(unique(coauthor_npairs$npapers)),
       col = "SkyBlue2", lwd = sort(unique(coauthor_npairs$npapers)))







### Paper authors network: ###########################################

##- The nodes represent papers with an edge between two papers if they
##- share at least one author; the node color indicates the conference
##- year.


el <- as.edgelist(papers_authors,
                  edge.var = "author",
                  node.var = "id")

years <- unique(papers_authors$year)

cols <- rainbow_hcl(length(years))
names(cols) <- as.character(years)

no <- data.frame(name = as.character(papers_authors$id),
                 color = cols[as.character(papers_authors$year)],
                 stringsAsFactors = FALSE)
no <- unique(no)


## Graph:
gr <- graph.data.frame(el, directed = FALSE, vertices = no)

set.seed(1234)
plot(gr, vertex.size = 3, vertex.label = NA,
     layout = layout.fruchterman.reingold)
legend("topleft", legend = years, pch = 19, col = cols)



