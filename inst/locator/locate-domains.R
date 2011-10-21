### Estimate the geolocation of each (email) domain and its
### higher-level domains.

library("stringr")

source("../scraper/xml.R")
source("geoiplookup.R")
source("domains.R")


isipta_domains <- function(files) {
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


scrap_domains_geolocation <- function(file) {
  domains <- isipta_domains(file)
  domains <- sapply(domains, basedomain)
  domains <- unique(domains)

  res <- xmlDomains()
  for ( domain in domains )
    res$addNode(xmlDomain(domain, domain_geolocation(domain)))

  res
}


### Scrap domains geolocation:

# files <- list.files("../xml/", pattern = "isipta.*", full.name = TRUE)
domains <- scrap_domains_geolocation("../xml/isipta2011.xml")
saveXML(domains$value(), file = "../xml/geoloc_domains.xml")
