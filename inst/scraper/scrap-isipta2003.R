
source("base-scraper.R")
source("xml.R")


scrap_paper <- function(url) {
  ## Raw values:
  raw <- readLines(url, encoding = "iso-8859-1")
  #raw <- iconv(raw, "latin1", "UTF-8")

  site <- htmlTreeParse(raw, useInternalNodes = TRUE,
                        encoding = "iso-8859-1")

  paperid <- sub(".*/a(\\d+)\\.html$", "\\1", url)
  authors <- xmlValue(getNodeSet(site, "/html/body/h2")[[1]])
  title <- xmlValue(getNodeSet(site, "/html/body/h1")[[1]])
  abstract <- xmlValue(getNodeSet(site, "/html/body/p[3]")[[1]])
  keywords <- xmlValue(getNodeSet(site, "/html/body/p[4]")[[1]])
  emails <- getNodeSet(site, "//table/tr",
                       fun = function(x) {
                         c(name = xmlValue(x[[1]]),
                           email = xmlValue(x[[2]]))
                       })

  free(site)


  ## Pdf file:
  title <- clean_title(title)
  pdf <- local({
    i <- agrep(tolower(title), tolower(sapply(PAPER_PDFS, "[[", 1)))
    if ( length(i) == 1 )
      PAPER_PDFS[[i]][2]
    else
      NULL
  })


  ## Clean XML structure:
  xmlPaper(paperid,
           title,
           clean_keywords(keywords),
           clean_abstract(abstract),
           clean_pdf(pdf),
           clean_authors(authors, emails))
}


PAPER_PDFS <- NULL
setup_paper_pdfs <- function() {
  ## Scrap pdf url for each paper:
  url <- "http://www.carleton-scientific.com/isipta/2003-toc.html"
  site <- htmlTreeParse(url, useInternalNodes = TRUE,
                        encoding = "iso-8859-1")

  PAPER_PDFS <<- getNodeSet(site, "//li/a",
                            fun = function(x) {
                              c(xmlValue(x), xmlAttrs(x)["tppabs"])
                            })

  free(site)
}


scrap_proceedings <- function(url, year, date, location) {
  setup_paper_pdfs()

  ## Papers URLs:
  site <- htmlTreeParse(sprintf("%sabstracts.html", url),
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



### ISIPTA 2003:

i03 <- scrap_proceedings("http://www.sipta.org/isipta03/proceedings/",
                         "2003",
                         c("2003-07-14", "2003-07-17"),
                         c(country_code = "CH",
                           country_name = "Switzerland",
                           city = "Lugano",
                           city_lat = "46.00651",
                           city_lon = "8.95231",
                           university = "University of Lugano",
                           department = ""))


saveXML(i03$value(), file = "../xml/isipta2003.xml")
