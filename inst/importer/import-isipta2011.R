
source("../scraper/xml.R")
source("base-importer.R")


import_paper <- function(paper) {
  n <- length(paper)

  paperid <- paper[1]
  title <- paper[3]

  tmp <- str_split(paper[4:n], ";")
  authors <- sapply(tmp, "[[", 1)
  emails <- sapply(tmp, "[[", 2)

  abstract <- ""
  keywords <- ""
  pdf <- ""

  xmlPaper(paperid,
           clean_title(title),
           clean_keywords(keywords),
           clean_abstract(abstract),
           clean_pdf(pdf),
           clean_authors(authors, emails))
}


import_proceedings <- function(file, year, date, location) {
  raw <- readLines(file)

  paper_begin <- head(c(1, which(raw == "##") + 1), -1)
  paper_end <- which(raw == "##") - 1
  paper_list <- Map(function(i, j) raw[i:j], paper_begin, paper_end)

  ## Add paper Id:
  paper_list <- lapply(seq(along = paper_list),
                       function(i) {
                         p <- paper_list[[i]]
                         c(sprintf("%03i", i), p)
                       })

  proc <- xmlProceedings(year, date, location)
  for ( paper in paper_list ) {
    print(paper[1])
    proc$addNode(import_paper(paper))
  }

  proc
}



### ISIPTA 2011:

i11 <- import_proceedings("../raw/dataisipta11",
                          "2011",
                          c("2011-07-25", "2011-07-28"),
                          c(country_name = "Austria",
                            country_code = "AT",
                            city = "Innsbruck",
                            city_lat = "47.267222",
                            city_lon = "11.392778",
                            university = "University of Innsbruck",
                            department = "Unit for Engineering Mathematics"))

saveXML(i11$value(), file = "../xml/isipta2011.xml")

