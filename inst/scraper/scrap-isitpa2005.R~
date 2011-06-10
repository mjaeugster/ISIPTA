
source("base-scraper.R")
source("xml.R")


scrap_paper <- function(url) {
  ## Raw values:
  raw <- readLines(url, encoding = "iso-8859-1")
  #raw <- iconv(raw, "latin1", "UTF-8")

  site <- htmlTreeParse(raw, useInternalNodes = TRUE,
                        encoding = "iso-8859-1")

  paperid <- sub(".*/(\\d+)\\.html$", "\\1", url)
  authors <- xmlValue(getNodeSet(site, "/html/body/h2")[[1]])
  title <- xmlValue(getNodeSet(site, "/html/body/h1")[[1]])
  abstract <- xmlValue(getNodeSet(site, "/html/body/p[3]")[[1]])
  keywords <- xmlValue(getNodeSet(site, "/html/body/p[4]")[[1]])
  pdf <- xmlAttrs(getNodeSet(site, "/html/body/ul/li[2]/a")[[1]])
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
  site <- htmlTreeParse(url, useInternalNodes = TRUE,
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



### ISIPTA 2005:

i05 <- scrap_proceedings("http://www.sipta.org/isipta05/proceedings/",
                         "2005",
                         c("2005-07-20", "2005-07-23"),
                         c(country_name = "USA",
                           city = "Pittsburgh",
                           university = "Carnegie Mellon University"))

saveXML(i05$value(), file = "../xml/isipta2005.xml")
