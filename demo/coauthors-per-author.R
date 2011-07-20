### Coauthors per author, i.e., collaboration.

library("ISIPTA")

data("papers_authors", package = "ISIPTA")

papers_ncoauthors <- ddply(papers_authors, .(id),
                          function(x) {
                            data.frame(year = x$year,
                                       id = x$id,
                                       author = x$author,
                                       ncoauthors = nrow(x) - 1)
                          })

papers_ncoauthors <- within(papers_ncoauthors, {
  year <- ordered(year)
  id <- factor(id)
})



### Numbers are proportional to authors per papers, or? ##############

source("authors-per-paper.R")

with(papers_ncoauthors, table(year, ncoauthors))
with(papers_ncoauthors, table(year, ncoauthors))


ggplot(papers_ncoauthors, aes(factor(ncoauthors))) + geom_bar()
ggplot(papers_ncoauthors, aes(factor(ncoauthors), fill = factor(year))) + geom_bar()
ggplot(papers_ncoauthors, aes(factor(year), fill = factor(ncoauthors))) + geom_bar()




### Overall distribtion: #############################################

papers_ncoauthors_overall <-
  ddply(papers_ncoauthors, .(author), numcolwise(sum))

ggplot(papers_ncoauthors_overall, aes(ordered(ncoauthors))) + geom_bar()



### Distribution by year: ############################################

ggplot(papers_ncoauthors, aes(ordered(ncoauthors))) +
    geom_bar() + facet_grid(year ~ .)


## -> see coauthor-network for a detailed analysis.

