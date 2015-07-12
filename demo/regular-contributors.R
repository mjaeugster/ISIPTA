### Regular contributors.

library("ISIPTA")

demo("simple-summary", package = "ISIPTA",
     verbose = FALSE, echo = FALSE, ask = FALSE)

data("authors_locations", package = "ISIPTA")

#converts the variable year into an ordered factor
authors_locations$year <- ordered(authors_locations$year)

conferences_contributors <-
  ddply(authors_locations, .(author),
        function(x) {
          data.frame(t(as.matrix(table(x$year))))
        })

colnames(conferences_contributors) <-
  c("author", sub("X", "ISIPTA", colnames(conferences_contributors)[-1]))


authors_ncontributions <-
  data.frame(author = conferences_contributors$author,
             ncontribs = rowSums(conferences_contributors[, -1]))



### Contribution "distribution": #####################################

t5 <- table(authors_ncontributions$ncontribs)
t5


ggplot(melt(t5, varnames = c("ncontribs")),
       aes(ordered(ncontribs), value)) + geom_bar(stat="identity")



### The "regular contributors": ######################################

nconferences <- nlevels(authors_locations$year)

subset(authors_ncontributions, ncontribs == nconferences)
subset(authors_ncontributions, ncontribs >= 6)



### Contributors flow: ###############################################

flow <- 2:nconferences
names(flow) <- paste(colnames(conferences_contributors)[-c(1, nconferences+1)],
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
