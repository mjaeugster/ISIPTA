\name{authors_locations}
\alias{authors_locations}
\docType{data}
\title{Estimated location of authors}
\description{
Estimated (by the authors' email domain) location of the ISIPTA authors.
}
\usage{data(authors_locations)}
\format{
  A data frame with 451 observations on the following 8 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{country_code}}{a factor with levels \code{} \code{AT} \code{BE} \code{BG} \code{BR} \code{CA} \code{CH} \code{CZ} \code{DE} \code{DK} \code{ES} \code{FR} \code{GB} \code{IE} \code{IL} \code{IP Address not found} \code{IT} \code{JP} \code{NL} \code{NZ} \code{RU} \code{SE} \code{SI} \code{US} \code{VE}}
    \item{\code{country_name}}{a factor with levels \code{} \code{Austria} \code{Belgium} \code{Brazil} \code{Bulgaria} \code{Canada} \code{Czech Republic} \code{Denmark} \code{France} \code{Germany} \code{Ireland} \code{Israel} \code{Italy} \code{Japan} \code{Netherlands} \code{New Zealand} \code{Russian Federation} \code{Slovenia} \code{Spain} \code{Sweden} \code{Switzerland} \code{United Kingdom} \code{United States} \code{Venezuela}}
    \item{\code{city}}{a factor with levels \code{} \code{Ames} \code{Amsterdam} \code{Atlanta} \code{Auckland} \code{Badajoz} \code{Barcelona} \code{Bayreuth} \code{Belfast} \code{Bellevue} \code{Berlin} \code{Berne} \code{Binghamton} \code{Bologna} \code{Bremen} \code{Bristol} \code{Brussels} \code{Caracas} \code{Catania} \code{Davis} \code{Durham} \code{Evanston} \code{Gaillimh} \code{Ghent} \code{Granada} \code{Graz} \code{Innsbruck} \code{Ithaca} \code{Jerez} \code{Jouy-en-josas} \code{Lawrence} \code{Le Chesnay} \code{Leiden} \code{Limerick} \code{Ljubljana} \code{Los Alamos} \code{Los Angeles} \code{Manchester} \code{Milan} \code{Minneapolis} \code{Modena} \code{Montpellier} \code{Montreal} \code{Mountain View} \code{Munich} \code{New York} \code{Newcastle Upon Tyne} \code{Oviedo} \code{Oxford} \code{Palermo} \code{Palma} \code{Paris} \code{Pasadena} \code{Perugia} \code{Piscataway} \code{Pittsburgh} \code{Pixel} \code{Ponta Grossa} \code{Potsdam} \code{Prague} \code{Provo} \code{Recife} \code{Rio De Janeiro} \code{Rome} \code{Roskilde} \code{Saint-denis} \code{Saint Gallen} \code{Salzburg} \code{Saskatoon} \code{Siena} \code{Southampton} \code{Stockholm} \code{Sundsvall} \code{Sunnyvale} \code{Tel Aviv} \code{Tempe} \code{Toronto} \code{Toulouse} \code{Trieste} \code{Tsukuba} \code{Urbana} \code{Vancouver} \code{Vandoeuvre} \code{Vienna} \code{Waltham} \code{West Long Branch}}
    \item{\code{city_lat}}{a numeric vector}
    \item{\code{city_lon}}{a numeric vector}
    \item{\code{university}}{a character vector}
    \item{\code{department}}{a character vector}
  }
}
\source{
Screen scraped from the ISIPTA website \url{http://www.sipta.org/}; see directories \code{scraper} and \code{xml} for the screen scrapers and the raw data.
}
\examples{
data(authors_locations)
## maybe str(authors_locations) ; plot(authors_locations) ...
}
\keyword{datasets}
\seealso{
\code{\link{papers_authors}}
}
