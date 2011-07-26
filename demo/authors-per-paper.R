### Authors per paper, i.e., collaboration.

library("ISIPTA")

data("papers_authors", package = "ISIPTA")


papers_nauthors <-
  ddply(papers_authors, .(id),
        function(x) {
          data.frame(year = x$year[1],
                     id = x$id[1],
                     nauthors = nrow(x))
        })


t4 <- table(papers_nauthors$year, papers_nauthors$nauthors)

t4



### Visualization of the frequencies: ################################

## Absolut numbers of authors per paper:
ggplot(papers_nauthors, aes(factor(nauthors), fill = factor(nauthors))) +
  geom_bar() + xlab("Authors per paper") + ylab("Papers") +
  no_legend()

ggsave("authors-per-paper-1.pdf", width = 7, height = 7, useDingbats = FALSE)


## Numbers of authors per paper per year:
ggplot(papers_nauthors, aes(factor(nauthors), fill = factor(year))) + geom_bar()
ggplot(papers_nauthors, aes(factor(year), fill = factor(nauthors))) + geom_bar()



### Visualization of the trend: ######################################

t4melt <- melt(t4, varnames = c("year", "nauthors"))


## ... grouped by year:
ggplot(t4melt, aes(factor(nauthors), value,
                   group = factor(year), colour = factor(year))) +
    geom_point() + geom_line()


## ... grouped by number of authors:
ggplot(t4melt, aes(factor(year), value,
                   group = factor(nauthors), colour = factor(nauthors))) +
  geom_point() + geom_line() +
  xlab("Year") + ylab("Papers") +
  labs(colour = "") +
  no_legend()

ggsave("authors-per-paper-2.pdf", width = 7, height = 7, useDingbats = FALSE)



