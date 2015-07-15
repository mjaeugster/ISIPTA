# distribution of unique coauthors

unique_coauthors <- data.frame(name = names(V(graph)), ncoauthors = ego_size(graph, order=1)-1)
coauthorsdist <- table(unique_coauthors$ncoauthors)

ggplot(melt(coauthorsdist, varnames = c("ncoauthors")),
       aes(ordered(ncoauthors), value)) + geom_bar(stat="identity") + xlab("... unique coauthors") + ylab("Number of authors with ...")

subset(unique_coauthors, ncoauthors == 14)

subset(unique_coauthors, ncoauthors >= 10)

# need cumulative coauthors by year!
