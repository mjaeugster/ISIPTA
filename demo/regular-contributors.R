### Regular contributors.

library("ISIPTA")

data("authors_locations", package = "ISIPTA")


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


authors_ncontributions <- data.frame(author = conferences_contributors$author,
                                     ncontribs = rowSums(conferences_contributors[, -1]))



### Contribution "distribution": #####################################

t5 <- table(authors_ncontributions$ncontribs)
t5


ggplot(melt(t5, varnames = c("ncontribs")),
       aes(ordered(ncontribs), value)) + geom_bar()



### The "regular contributors": ######################################

nconferences <- nlevels(authors_locations$year)

subset(authors_ncontributions, ncontribs == nconferences)



### Contributors flow: ###############################################

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
source("simple-summary.R")

max_loss <- sapply(flow,
                   function(i) {
                     min(t1$unique_authors[i-1], t1$unique_authors[i])
                   })

colSums(contributors_flow) / max_loss
