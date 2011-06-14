\name{as.edgelist}
\alias{as.edgelist}
\title{Create an edgelist matrix from a data frame}
\description{
  Create an edgelist matrix from a data frame.
}
\usage{
  as.edgelist(x, edge.var, node.var)
}
\arguments{
  \item{x}{Data set (\\code{data.frame} in long format) to convert into
    the edgelist matrix.}
  \item{edge.var}{Edge variable within the data set.}
  \item{node.var}{node.var Node variable within the data set.}
}
\value{Edgelist matrix with two columns (vertices) and one row per
  edge.}
\examples{
  ## Papers as nodes, co-authors as edges:
  data("papers_authors")
  as.edgelist(papers_authors,
              edge.var = "author",
              node.var = "id")


  ## Authors as nodes, papers as edges:
  data("papers_authors")
  as.edgelist(papers_authors,
              edge.var = "id",
              node.var = "author")


  ## Keywords as nodes, papers as edges:
  data("papers_keywords")
  as.edgelist(papers_keywords,
              edge.var = "id",
              node.var = "keyword")
}
