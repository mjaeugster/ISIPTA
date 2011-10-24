\name{papers}
\alias{papers}
\docType{data}
\title{Titles of the ISIPTA papers}
\description{
  Titles of the ISIPTA papers; abstracts are available in the XML file
  (see directory \code{xml}).
}
\usage{data(papers)}
\format{
  A data frame with 323 observations on the following 3 variables.
  \describe{
    \item{\code{year}}{Conference year}
    \item{\code{id}}{Paper ID}
    \item{\code{title}}{Paper title}
  }
}
\source{
  Screen scraped from the ISIPTA website \url{http://www.sipta.org/};
  see file \code{NOTES} in the package directory for detailed
  information.
}
\examples{
data(papers)
}
\keyword{datasets}
\seealso{
\code{\link{papers_authors}}, \code{\link{authors_locations}}, \code{\link{papers_keywords}}
}
