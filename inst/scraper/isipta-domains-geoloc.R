### Estimate the geolocation of each (email) domain and its
### higher-level domains.

library("stringr")

source("geoiplookup.R")
source("xml.R")
source("domains.R")


isipta_domains <- function() {
  files <- list.files("../xml/", pattern = "isipta.*", full.name = TRUE)

  domains <- lapply(files,
                    function(file) {
                      raw <- readLines(file, encoding = "UTF-8")
                      xml <- xmlTreeParse(raw, useInternalNodes = TRUE)
                      domains <- getNodeSet(xml, "//email", fun = domain)
                      free(xml)
                      domains
                    })

  unique(unlist(domains))
}


domain_geolocation <- function(x) {
  loc <- geoiplookup(x)

  if ( is.null(loc) )
    loc <- geoiplookup(sprintf("www.%s", x))

  loc
}


isipta_domains_geolocation <- function() {
  domains <- isipta_domains()
  domains <- sapply(domains, basedomain)
  domains <- unique(domains)

  res <- xmlDomains()
  for ( domain in domains )
    res$addNode(xmlDomain(domain, domain_geolocation(domain)))

  res
}


domains <- isipta_domains_geolocation()
saveXML(domains$value(), file = "../xml/geoloc_domains.xml")
