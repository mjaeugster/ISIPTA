\name{papers_keywords}
\alias{papers_keywords}
\docType{data}
\title{Keywords of the ISIPTA papers}
\description{
Keywords of the ISIPTA papers.
}
\usage{data(papers_keywords)}
\format{
  A data frame with 1418 observations on the following 3 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{id}}{a numeric vector}
    \item{\code{keyword}}{a character vector}
  }
}
\source{
Screen scraped from the ISIPTA website \url{http://www.sipta.org/}; see directories \code{scraper} and \code{xml} for the screen scrapers and the raw data.
}
\examples{
data(papers_keywords)
## maybe str(papers_keywords) ; plot(papers_keywords) ...
}
\keyword{datasets}
\seealso{
\code{\link{papers}}, \code{\link{authors_locations}}, \code{\link{papers_authors}}
}
