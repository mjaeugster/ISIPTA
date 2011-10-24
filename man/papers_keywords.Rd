\name{papers_keywords}
\alias{papers_keywords}
\docType{data}
\title{Keywords of the ISIPTA papers}
\description{
  Keywords of the ISIPTA papers.
}
\usage{data(papers_keywords)}
\format{
  A data frame with 1458 observations on the following 3 variables.
  \describe{
    \item{\code{year}}{Conference year}
    \item{\code{id}}{Paper ID}
    \item{\code{keyword}}{Keyword}
  }
}
\source{
  Screen scraped from the ISIPTA website \url{http://www.sipta.org/};
  see file \code{NOTES} in the package directory for detailed
  information.
}
\examples{
data(papers_keywords)
}
\keyword{datasets}
\seealso{
\code{\link{papers}}, \code{\link{authors_locations}}, \code{\link{papers_authors}}
}
