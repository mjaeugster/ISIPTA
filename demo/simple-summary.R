### Simple summary numbers by years.

library("ISIPTA")

data("papers", package = "ISIPTA")
data("papers_authors", package = "ISIPTA")


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

t1



### Visualization by year: ###########################################

t1melt <- melt(t1, id = "year")
t1melt$year <- ordered(t1melt$year)

ggplot(t1melt, aes(year, value, group = variable, colour = variable)) +
    geom_point() + geom_line()
