\name{conferences}
\alias{conferences}
\docType{data}
\title{ISIPTA conference facts}
\description{
Dates, locations, etc. about the ISIPTA conferences.
}
\usage{data(conferences)}
\format{
  A data frame with 7 observations on the following 10 variables.
  \describe{
    \item{\code{year}}{Conference year}
    \item{\code{country_code}}{Country code}
    \item{\code{country_name}}{Country name}
    \item{\code{city}}{City name}
    \item{\code{city_lat}}{City latitude}
    \item{\code{city_lon}}{City longitude}
    \item{\code{university}}{Universtiy}
    \item{\code{department}}{Department}
    \item{\code{date_start}}{Conference beginning date}
    \item{\code{date_end}}{Conference ending date}
  }
}
\source{
  Screen scraped from the ISIPTA website \url{http://www.sipta.org/};
  see file \code{NOTES} in the package directory for detailed
  information.
}
\examples{
data(conferences)
}
\keyword{datasets}
\seealso{
\code{\link{papers}}
}
