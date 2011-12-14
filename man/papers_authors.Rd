\name{papers_authors}
\alias{papers_authors}
\docType{data}
\title{Authors of the ISIPTA papers}
\description{
  Authors of the ISIPTA papers; the names are normalized to ASCII characters.
}
\usage{data(papers_authors)}
\format{
  A data frame with 608 observations on the following 3 variables.
  \describe{
    \item{\code{year}}{Conference year}
    \item{\code{id}}{Paper ID}
    \item{\code{author}}{Author name}
  }
}
\source{
  Screen scraped from the ISIPTA website \url{http://www.sipta.org/};
  see file \code{NOTES} in the package directory for detailed
  information.
}
\examples{
data(papers_authors)
}
\keyword{datasets}
\seealso{
\code{\link{papers}}, \code{\link{authors_locations}}, \code{\link{papers_keywords}}
}
