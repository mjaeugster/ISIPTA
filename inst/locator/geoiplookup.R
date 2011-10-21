### geoiplookup interface for geotargeting of domain names
###
### Data used from the MaxMind open source databases via the
### geoiplookup (libgeo1) command line tool.
###
### http://www.maxmind.com/app/ip-location


geoiplookup <- function(domain) {
  cmd <- sprintf("geoiplookup %s", domain)
  res <- system(cmd, intern = TRUE)

  parse.geoiplookup(res)
}


parse.geoiplookup <- function(x) {
  if ( grepl("can't resolve hostname", x[1]) )
    return(NULL)

  ## Country information:
  country <- sub("GeoIP Country Edition: ", "", x[1])
  country <- strsplit(country, ", ")[[1]]

  ## City information:
  city <- sub("GeoIP City Edition, .*: ", "", x[2])
  city <- strsplit(city, ", ")[[1]]


  c(country_code = country[1],
    country_name = country[2],
    city = city[3],
    city_lat = city[5],
    city_lon = city[6])
}

