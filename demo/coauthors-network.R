### Coauthors network.

data("papers_authors", package = "ISIPTA")

coauthors_pairs <- ddply(papers_authors, .(id),
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

coauthors_npairs <- ddply(coauthors_pairs, .(author1, author2),
                          function(x) {
                            c(x$author[1],
                              x$author[2],
                              npapers = nrow(x))
                          })


