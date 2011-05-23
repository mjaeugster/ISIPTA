
source("base-scraper.R")


scrap_paper <- function(url) {
  ## Raw values:
  site <- htmlTreeParse(url, useInternalNodes = TRUE,
                        encoding = "iso-8859-1")

  author <- xmlValue(getNodeSet(site, "/html/body/h2[2]/font")[[1]])
  title <- xmlValue(getNodeSet(site, "/html/body/h1/font")[[1]])
  abstract <- xmlValue(getNodeSet(site, "/html/body/p[4]")[[1]])
  keywords <- xmlValue(getNodeSet(site, "/html/body/p[5]")[[1]])
  pdf <- xmlAttrs(getNodeSet(site, "/html/body/ul/li[2]/a")[[1]])
  email <- getNodeSet(site, "//table/tr",
                      fun = function(x) {
                        c(name = xmlValue(x[[1]]),
                          email = xmlValue(x[[2]]))
                      })
  
  free(site)

  
  ## Clean values:
  title <- str_trim(title)

  keywords <- local({
    n <- str_locate(keywords, "Keywords.")[1, "end"] + 1
    tmp <- str_sub(keywords, start = n)
    tmp <- str_trim(str_split(tmp, ",")[[1]])
    tmp <- str_replace_all(tmp, "\\.$", "")
    tmp
  })

  abstract <- str_trim(abstract)
  pdf <- unname(pdf)
  author <- str_trim(do.call(rbind, email))

  
}


scrap_proceedings <- function(url) {
  site <- htmlTreeParse(url, useInternalNodes = TRUE,
                        encoding = "iso-8859-1")

  paper_urls <- getNodeSet(site, "//a/font[@color='555500']/..",
                           fun = xmlAttrs)
  paper_urls <- paste(url, paper_urls, sep = "")

  free(site)

  lapply(paper_urls, scrap_paper)
}

url <- "http://www.sipta.org/isipta09/proceedings/"
