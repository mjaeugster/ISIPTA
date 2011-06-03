
source("base-scraper.R")


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

  paper_urls <- getNodeSet(site, "//dt/font/a",
                           fun = xmlAttrs)
  paper_urls <- paste(url, paper_urls, sep = "")

  free(site)


  ## Scrap each paper:
  proc <- xmlProceedings(year, date, location)
  for ( url in paper_urls )
    proc$addNode(scrap_paper(url))

  proc
}



### ISIPTA 2001:

i01 <- scrap_proceedings("http://www.sipta.org/isipta01/proceedings/",
                         "2001",
                         c("2001-06-26", "2001-06-29"),
                         c("USA", "Ithaca",
                           "Cornell University",
                           ""))

saveXML(i01$value(), file = "../xml/isipta2001.xml")
