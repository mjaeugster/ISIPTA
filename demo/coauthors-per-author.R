### Coauthors per author. See demo("coauthor-network") for a detailed
## analysis on the unique pairs of coauthors.

library("ISIPTA")

demo("authors-per-paper", package = "ISIPTA",
     verbose = FALSE, echo = FALSE, ask = FALSE)

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

