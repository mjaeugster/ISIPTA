### Papers per author, i.e., productivity.

library("ISIPTA")

data("papers_authors", package = "ISIPTA")


authors_npapers <-
  ddply(papers_authors, .(author, year),
        function(x) {
          data.frame(year = x$year[1],
                     author = x$author[1],
                     npapers = nrow(x))
        })

authors_npapers$year <- ordered(authors_npapers$year)


authors_npapers_overall <-
  ddply(authors_npapers, .(author), numcolwise(sum))



### Oveall papers per author "distribution": #########################

t5 <- table(authors_npapers_overall$npapers)
t5


ggplot(melt(t5, varnames = c("npapers")),
       aes(ordered(npapers), value)) + geom_bar()


## Who are these high productive authors?
subset(authors_npapers_overall, npapers > 6)



### Maximum number of papers per author per year: ####################

ddply(authors_npapers, .(year), numcolwise(max))


## Who?
ddply(authors_npapers, .(year),
      function(x) {
        subset(x, npapers == max(x$npapers))
      })



### Mean papers per author: ##########################################

ddply(authors_npapers, .(year), numcolwise(mean))
ddply(authors_npapers, .(year), numcolwise(median))
ddply(authors_npapers, .(year), numcolwise(sd))

