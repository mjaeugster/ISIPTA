
source("base-scraper.R")
source("xml.R")


scrap_proceedings11 <- function(url, year, date, location) {
  ## Papers URLs:
  site <- htmlTreeParse(paste(url,"index.php?id=list_of_papers",sep=""), useInternalNodes = TRUE,
                        encoding = "iso-8859-1")
  
  paper_urls <- getNodeSet(site, "//dt/a",
                           fun = xmlAttrs)
  paper_urls <- paste(url, paper_urls, sep = "")
  
  free(site) #OK
  
  
  ## Scrap each paper:
  proc <- xmlProceedings(year, date, location) # produces warning, but seems ok
  for ( url in paper_urls )
    proc$addNode(scrap_paper11(url))
  
  proc
} 

scrap_paper11 <- function(url2) {
  ## Raw values:
  raw <- readLines(url2, encoding = "iso-8859-1")
  #raw <- iconv(raw, "latin1", "UTF-8")
  
  site <- htmlTreeParse(raw, useInternalNodes = TRUE,
                        encoding = "iso-8859-1")
  
  paperid <- sub(".*=(\\d+)\\.html", "\\1", url2) # need number between "=" and ".html"
  authors <- xmlValue(getNodeSet(site, "/html/body/div/div/h3[1]")[[1]]) #OK
  title <- xmlValue(getNodeSet(site, "/html/body/div/div/h2")[[1]])      #OK
  abstract <- xmlValue(getNodeSet(site, "/html/body/div/div/p[3]")[[1]]) #OK
  keywordspart <- xmlValue(getNodeSet(site, "/html/body/div/div")[[1]])
  ###keywords <- sub(".*Keywords(.*)\\.\n*(Download area.*)", "\\1", keywordspart) #OK for paper_urls[1]
  #keywords <- sub(".*Keywords(.*)Download area(.*)", "\\1", keywordspart)     #OK for paper_urls[1]-[2]
  keywords <- sub(".*(Keywords.*)Download area(.*)", "\\1", keywordspart)     #OK for paper_urls[1]-[2]
  pdf <- xmlAttrs(getNodeSet(site, "/html/body/div/div/ul/li[2]/a")[[1]])
  emails <- getNodeSet(site, "//table/tr",
                       fun = function(x) {
                         c(name = xmlValue(x[[1]]),
                           email = xmlValue(x[[3]]))
                       })                                                #OK
  
  free(site)
  
  
  ## Clean XML structure:
  xmlPaper(paperid,
           clean_title(title),
           clean_keywords(keywords),
           clean_abstract(abstract),
           clean_pdf(pdf),
           clean_authors(authors, emails)
  )
}


### ISIPTA 2011:

i11 <- scrap_proceedings11("http://www.sipta.org/isipta11/",
                         "2011",
                         c("2011-07-25", "2011-07-28"),
                         c(country_name = "Austria",
                           country_code = "AT",
                           city = "Innsbruck",
                           city_lat = "47.266667",
                           city_lon = "11.383333",
                           university = "University of Innsbruck",
                           department = "Unit for Engineering Mathematics"))

saveXML(i11$value(), file = "../xml/isipta2011new.xml")
