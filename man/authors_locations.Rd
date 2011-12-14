\name{authors_locations}
\alias{authors_locations}
\docType{data}
\title{Estimated location of the authors}
\description{
  Estimated (by the authors' email domains and their geotargeting using
  GeoIP with databases by MaxMind
  \url{http://www.maxmind.com/app/ip-location}) location of the ISIPTA
  authors.
}
\usage{data(authors_locations)}
\format{
  A data frame with 531 observations on the following 9 variables.
  \describe{
    \item{\code{year}}{Conference year}
    \item{\code{author}}{Author name}
    \item{\code{country_code}}{Country code}
    \item{\code{country_name}}{Country name}
    \item{\code{city}}{City name}
    \item{\code{city_lat}}{City latitude}
    \item{\code{city_lon}}{City longitude}
    \item{\code{university}}{University (currently \code{NA})}
    \item{\code{department}}{Department (currently \code{NA})}
  }
}
\source{
  Screen scraped from the ISIPTA website \url{http://www.sipta.org/};
  see file \code{NOTES} in the package directory for detailed
  information.
}
\examples{
data(authors_locations)
}
\keyword{datasets}
\seealso{
\code{\link{papers_authors}}
}
