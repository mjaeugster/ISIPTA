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

flow3 <- 4:nconferences
names(flow3) <- paste(colnames(conferences_contributors[, -c(1:4)]), "3", sep = "-")

contributors_flow3 <- sapply(flow3,
                             function(i) {
                               i <- i + 1 # as first column is "author"
                               as.logical(conferences_contributors[, i]) &
                                 (as.logical(conferences_contributors[, i-1]) |
                                  as.logical(conferences_contributors[, i-2]) |
                                  as.logical(conferences_contributors[, i-3]))
                             })
colSums(contributors_flow3)

t1new <- cbind(t1, flow1 = c(NA, colSums(contributors_flow)),
                   flow2 = c(NA, NA, colSums(contributors_flow2)))
rownames(t1new) <- NULL
names(t1new) <- c("year", "Papers", "Paper authors", "Unique authors", "1-step flow", "2-step flow")

t1newmelt <- melt(t1new, id = "year")
t1newmelt$year <- ordered(t1newmelt$year)
t1newmelt <- subset(t1newmelt, !is.na(value))

bottomlegend <- theme(legend.position = 'bottom', legend.direction = 'horizontal', legend.title = element_blank())
rightlegend <- theme(legend.title = element_blank())

pdf("./../../1-summary-flow.pdf", width=6, height=6)
ggplot(t1newmelt, aes(year, value, group = variable, colour = variable)) +
  geom_point() + geom_line() + bottomlegend + xlab("") + ylab("Number of ...")
dev.off()  

names(t1) <- c("year", "Papers", "Paper authors", "Unique authors")
t1melt <- melt(t1, id = "year")
t1melt$year <- ordered(t1melt$year)

pdf("./../../1-summary.pdf", width=4.5, height=4.5)
ggplot(t1melt, aes(year, value, group = variable, colour = variable)) +
  geom_point() + geom_line() + bottomlegend + xlab("Year") + ylab("Number of ...")
dev.off()  


# 2. Author flow
# when was each author a new author?
head(authors_years)
# gives the conference number
newauthor <- apply(authors_years, 1, function(x) sum(x[-1] == 0)+1 )
newauthor <- data.frame(author = authors_years$author, confnumber = newauthor)
newyear <- newauthor$confnumber*2 + 1997
newauthor <- data.frame(newauthor, year=newyear)

authorflow <- data.frame(year = t1$year,
                         newauthor = as.numeric(table(newauthor$year)),
                         flow1 = c(NA, colSums(contributors_flow)),
                         flow2 = c(NA, NA, colSums(contributors_flow2)),
                         flow3 = c(NA, NA, NA, colSums(contributors_flow3)))
rownames(authorflow) <- NULL
authorflow[1,2] <- NA
names(authorflow) <- c("year", "New", "1-step", "2-step", "3-step") # recurring authors
authorflowmelt <- melt(authorflow, id = "year")
authorflowmelt$year <- ordered(authorflowmelt$year)
authorflowmelt <- subset(authorflowmelt, !is.na(value))

pdf("./../../2-flow.pdf", width=4.5, height=4.5)
ggplot(authorflowmelt, aes(year, value, group = variable, colour = variable)) +
  geom_point() + geom_line() + bottomlegend + xlab("Year") + ylab("Number of recurring authors")
dev.off()  


# 3. Authors per paper
t4new <- t4
#dimnames(t4new)[[2]] <- c("1 author", paste(2:5,"authors", sep=" "))
t4newmelt <- melt(t4new, varnames = c("year", "Authors"))
t4newmelt <- within(t4newmelt, {
  Authors <- ordered(Authors)
  year = ordered(year)
})

pdf("./../../3-authors-per-paper.pdf", width=4.5, height=4.5)
ggplot(t4newmelt, aes(year, value, group = Authors, colour = Authors)) +
  geom_point() + geom_line() + xlab("Year") + ylab("Number of papers with ...") +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') #+
  #  guides(group=guide_legend(title = "Number of authors"))
dev.off()  








# 6. Coauthor network

#networkseed <- 1234
#networkseed <- 2015
networkseed <- 999999
### big graph 2013 with rownames(vertices), i.e. rownumber as id
set.seed(networkseed)
#coords <- layout_(graph, with_graphopt(charge=0.01)) 
#coords <- layout_components(graph, layout = layout_with_fr)
coords <- layout_with_fr(graph, niter = 2000)
pdf("../../6-network2013.pdf", width=18, height=18)
op1 <- par(mar = c(1, 0, 0, 0))
plot(graph,
     vertex.size = 3.5,
     vertex.color = "gray90",
     vertex.label = rownames(vertices),
     vertex.frame.color = "gray90",
     vertex.label.color = "black",
     edge.color = "SkyBlue2",
     layout = coords)
#     layout = layout.fruchterman.reingold)
par(op1)
dev.off()

ipit <- find_node(c(105,30,52,254,106,180,179,13,87,104))
subset(authors_locations, author %in% ipit$name)
subset(authors_npapers_overall, author %in% ipit$name)

ipse <- find_node(c(56,8,192,172,150,274,257,42,146,206,27))
subset(authors_npapers_overall, author %in% ipse$name)
subset(authors_locations, author %in% ipse$name)

### Graphs over time:
years <- levels(coauthors_pairs$year)
years <- sapply(years, grep,
                colnames(coauthors_years), value = TRUE)
#years <- years[-length(years)] # to exclude latest graph

years1 <- years[1:4]
pdf("../../6-network1999-2005.pdf", width=24, height=6)
op <- par(mfrow = c(1, length(years1)))
for ( i in years1 ) {
  ewidth <- coauthors_years[[i]]
  ecolor <- ifelse(coauthors_years[[i]] > 0, "SkyBlue2", NA)
  vcolor <- ifelse(authors_years[[i]] > 0, "black", NA)
  fcolor <- ifelse(authors_years[[i]] > 0, "black", NA)
  
  op1 <- par(mar = c(1, 0, 0, 0))
  set.seed(networkseed)
  plot(graph,
       vertex.size = 3,
       vertex.label = NA,
       vertex.color = vcolor,
       vertex.frame.color = fcolor,
       edge.color = ecolor,
       edge.width = ewidth,
       layout = coords)
  #layout = layout.fruchterman.reingold)
  #mtext(i, side = 1, line = 0)
  par(op1)
}
par(op)
dev.off()

years2 <- years[5:7]
pdf("../../6-network2007-2011.pdf", width=6, height=18)
op <- par(mfrow = c(length(years2), 1))
for ( i in years2 ) {
  ewidth <- coauthors_years[[i]]
  ecolor <- ifelse(coauthors_years[[i]] > 0, "SkyBlue2", NA)
  vcolor <- ifelse(authors_years[[i]] > 0, "black", NA)
  fcolor <- ifelse(authors_years[[i]] > 0, "black", NA)
  
  op1 <- par(mar = c(1, 0, 0, 0))
  set.seed(networkseed)
  plot(graph,
       vertex.size = 3,
       vertex.label = NA,
       vertex.color = vcolor,
       vertex.frame.color = fcolor,
       edge.color = ecolor,
       edge.width = ewidth,
       layout = coords)
  #layout = layout.fruchterman.reingold)
  #mtext(i, side = 1, line = 0)
  par(op1)
}
par(op)
dev.off()

# 6.1 distribution of unique coauthors

unique_coauthors <- data.frame(name = names(V(graph)), ncoauthors = ego_size(graph, order=1)-1)
coauthorsdist <- table(unique_coauthors$ncoauthors)

pdf("../../6-distcoauthors.pdf", width=4.5, height=4.5)
ggplot(melt(coauthorsdist, varnames = c("ncoauthors")),
       aes(ordered(ncoauthors), value)) + geom_bar(stat="identity") +
      xlab("... unique coauthors") + ylab("Number of authors with ...")
dev.off()

subset(unique_coauthors, ncoauthors == 14)

subset(unique_coauthors, ncoauthors >= 10)

# need cumulative coauthors by year!

# asked by Lewis: who is the author with the most papers that has no coauthors?
loneauthors_npapers <- subset(authors_npapers_overall, author %in% subset(unique_coauthors, ncoauthors == 0)$name)
table(loneauthors_npapers$npapers)
subset(loneauthors_npapers, npapers >= 3)

#