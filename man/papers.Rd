\name{papers}
\alias{papers}
\docType{data}
\title{Conference papers}
\description{
Titles of the ISIPTA papers.
}
\usage{data(papers)}
\format{
  A data frame with 283 observations on the following 3 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{id}}{a numeric vector}
    \item{\code{title}}{a character vector}
  }
}
\source{
Screen scraped from the ISIPTA website \url{http://www.sipta.org/}; see directories \code{scraper} and \code{xml} for the screen scrapers and the raw data.
}
\examples{
data(papers)
## maybe str(papers) ; plot(papers) ...
}
\keyword{datasets}
\seealso{
\code{\link{papers_authors}}, \code{\link{authors_locations}}, \code{\link{papers_keywords}}
}
