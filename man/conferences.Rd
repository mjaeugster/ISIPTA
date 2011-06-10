\name{conferences}
\alias{conferences}
\docType{data}
\title{Conference facts}
\description{
Dates, locations, etc. about the ISIPTA conferences.
}
\usage{data(conferences)}
\format{
  A data frame with 6 observations on the following 10 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{country_code}}{a factor with levels \code{BE} \code{CH} \code{CZ} \code{GB} \code{US}}
    \item{\code{country_name}}{a factor with levels \code{Belgium} \code{Czech Republic} \code{Switzerland} \code{United Kingdom} \code{United States}}
    \item{\code{city}}{a character vector}
    \item{\code{city_lat}}{a numeric vector}
    \item{\code{city_lon}}{a numeric vector}
    \item{\code{university}}{a character vector}
    \item{\code{department}}{a character vector}
    \item{\code{date_start}}{a Date}
    \item{\code{date_end}}{a Date}
  }
}
\source{
Screen scraped from the ISIPTA website \url{http://www.sipta.org/}; see directories \code{scraper} and \code{xml} for the screen scrapers and the raw data.
}
\examples{
data(conferences)
## maybe str(conferences) ; plot(conferences) ...
}
\keyword{datasets}
\seealso{
\code{\link{papers}}
}
