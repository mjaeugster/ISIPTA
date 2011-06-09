
source("base-scraper.R")
source("xml.R")


scrap_paper <- function(url) {
  ## Raw values:
  raw <- readLines(url, encoding = "iso-8859-1")
  #raw <- iconv(raw, "latin1", "UTF-8")

  site <- htmlTreeParse(raw, useInternalNodes = TRUE,
                        encoding = "iso-8859-1") # UTF-8

  paperid <- sub(".*/(\\d+)\\.html$", "\\1", url)
  authors <- xmlValue(getNodeSet(site, "/html/body/h2[2]")[[1]])
  title <- xmlValue(getNodeSet(site, "/html/body/h1")[[1]])
  abstract <- xmlValue(getNodeSet(site, "/html/body/p[3]")[[1]])
  keywords <- xmlValue(getNodeSet(site, "/html/body/p[4]")[[1]])

  pdf <- getNodeSet(site, "/html/body/ul/li/a")
  pdf <- tail(sapply(pdf, xmlAttrs), 1)

  emails <- getNodeSet(site, "//table/tr",
                       fun = function(x) {
                         c(name = xmlValue(x[[1]]),
                           email = xmlValue(x[[2]]))
                       })

  free(site)


  ## Clean XML structure:
  xmlPaper(paperid,
           clean_title(title),
           clean_keywords(keywords),
           clean_abstract(abstract),
           clean_pdf(pdf),
           clean_authors(authors, emails))
}


scrap_proceedings <- function(url, year, date, location) {
  ## Papers URLs:
  site <- htmlTreeParse(sprintf("%sproceedings.html", url),
                        useInternalNodes = TRUE,
                        encoding = "iso-8859-1")

  paper_urls <- getNodeSet(site, "//dt/a",
                           fun = xmlAttrs)
  paper_urls <- paste(url, paper_urls, sep = "")

  free(site)


  ## Scrap each paper:
  proc <- xmlProceedings(year, date, location)
  for ( url in paper_urls )
    proc$addNode(scrap_paper(url))

  proc
}



### ISIPTA 1999:

i99 <- scrap_proceedings("http://decsai.ugr.es/~smc/isipta99/proc/",
                         "1999",
                         c("1999-06-30", "1999-07-02"),
                         c(country_name = "Belgium",
                           city = "Ghent"))

saveXML(i99$value(), file = "../xml/isipta1999.xml")
