### Number of unqiue authors by year and country.

library("ISIPTA")
library("rworldmap")

data("authors_locations", package = "ISIPTA")


t2 <- table(authors_locations$year, authors_locations$country_code)
t2



### Absolute numbers on the world map: ###############################

t2absolut <- data.frame(country_code = colnames(t2),
                        value = colSums(t2))

t2map <- joinCountryData2Map(t2absolut, joinCode = "ISO2",
                             nameJoinColumn = "country_code")

values <- seq(min(t2absolut$value), max(t2absolut$value))
pal <- rev(sequential_hcl(length(values), power = 1.2))


## World:
mapCountryData(t2map, nameColumnToPlot = "value",
               colourPalette = pal,
               catMethod = values,
               mapTitle = "",
               addLegend = TRUE,
               oceanCol = gray(0.95),
               missingCountryCol = "white")

## Europe:
mapCountryData(t2map, nameColumnToPlot = "value",
               mapRegion = "europe",
               colourPalette = pal,
               catMethod = values,
               mapTitle = "",
               addLegend = TRUE,
               oceanCol = gray(0.95),
               missingCountryCol = "white")



### Visualization by region and year: ################################

data("countryRegions", package = "rworldmap")

t2melt <- melt(t2, varnames = c("year", "country_code"))
# countryRegions now has only ISO3 country codes,
# so we must convert the two-letter ISO2 to the three-letter ISO3 code
t2melt$country_code3 <- NA
# isoToName() does not like character vectors.
# this loop throws 8 warnings but the result still seems fine (?)
for (i in 1:dim(t2melt)[1]){
  t2melt$country_code3[i] <- isoToName(iso=as.character(t2melt$country_code[i]), nameColumn='ISO_A3')
}
t2melt$region <- countryRegions[match(t2melt$country_code3,
                                      countryRegions$ISO3), "GEO3major"]
t2melt$region <- t2melt$region[, drop = TRUE]
t2melt$year <- ordered(t2melt$year)

t2melt <- ddply(t2melt, .(year, region), numcolwise(sum))


ggplot(t2melt, aes(year, value, group = region, colour = region)) +
  geom_point() + geom_line()

