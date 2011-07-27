\name{find_author}
\alias{find_author}
\alias{find_node}
\alias{summarize_author}
\title{
  Find and summarize an ISIPTA author
}
\description{
  Find an ISIPTA author for a given pattern; and summarize the author's
  ISIPTA contributions.
}
\usage{
find_author(pattern)
find_node(nodeid)
summarize_author(name, show.papers = FALSE)
}
\arguments{
  \item{pattern}{Part of an author name}
  \item{nodeid}{Node ID as shown in the graph (see
    \code{demo("coauthors-network")})}
  \item{name}{Exact author name as returned by \code{find_author}}
  \item{show.papers}{Display paper details; default is FALSE}
}
\value{
  \code{find_author} returns a \code{data.frame} with each author
  matching the \code{pattern} and the corresponding graph node id (see
  \code{demo("coauthors-network")}.

  \code{summarize_author} returns a list with information on the author;
  \code{print.ISIPTA_author} nicely prints the result.
}
\examples{
find_author("Gero")
summarize_author("Gero Walter")
summarize_author("Gero Walter", show.papers = TRUE)
find_node(81:83)
}
