# code for plots and tables for 2015 poster

# 1. summary statistics and author flow

flow2 <- 3:nconferences
names(flow2) <- paste(colnames(conferences_contributors[, -c(1:3)]), "2", sep = "-")

contributors_flow2 <- sapply(flow2,
                             function(i) {
                               i <- i + 1 # as first column is "author"
                               as.logical(conferences_contributors[, i]) &
                               (as.logical(conferences_contributors[, i-1]) |
                                as.logical(conferences_contributors[, i-2]))
                             })

## Number of authors contributing in conference i that have contributed in conference i-1 or i-2:
colSums(contributors_flow2)

t1new <- cbind(t1, flow1 = c(NA, colSums(contributors_flow)),
                   flow2 = c(NA, NA, colSums(contributors_flow2)))
rownames(t1new) <- NULL
names(t1new) <- c("year", "Papers", "Paper authors", "Unique authors", "1-step flow", "2-step flow")

t1newmelt <- melt(t1new, id = "year")
t1newmelt$year <- ordered(t1newmelt$year)
t1newmelt <- subset(t1newmelt, !is.na(value))

bottomlegend <- theme(legend.position = 'bottom', legend.direction = 'horizontal', legend.title = element_blank())

pdf("./../../1-summary-flow.pdf", width=6, height=6)
ggplot(t1newmelt, aes(year, value, group = variable, colour = variable)) +
  geom_point() + geom_line() + bottomlegend
dev.off()  


#