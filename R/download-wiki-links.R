##' Download XML-file with links using the Wiki API
##'
##' This Function uses the Wikipedia API to download the properties
##' (given by parameter prop) of an article. The resulting XML-files
##' are limited in size by the API. Therefore multiple requests have to
##' be made and the result is a list of XML documents. 
##' @title get_xml_files
##' @param title A string with the title of the article
##' @param prop The property of the article, which should be requested(e.g. links)
##' @return A vector containing plain character xml documents
##' @author Christoph Molnar
get_xml_files <- function(title, prop = "links"){
  query_complete <- FALSE
  plcontinue <- character(0)
  docs <- vector()
  i <- 1
  ##  Queries are limited; sometimes multiple queries have to be made
  while(!query_complete) {
    print(paste("Download number:", i))
    link <- paste("http://en.wikipedia.org/w/api.php?action=query&format=xml&prop=", prop, "&titles=",
                  title, "&pllimit=500&redirects", plcontinue, sep = "")
    link <- gsub(" ", "%", link)
    doc <- getURLContent(link, .opts=list(useragent = getOption("HTTPUserAgent")))
    parsed_doc <- xmlParse(doc)
    docs[i] <- doc
    continue <- getNodeSet(parsed_doc, "//*/query-continue/links")
    #browser()
    ifelse(XML:::length.XMLNode(continue) == 0,
           query_complete <- TRUE,
           plcontinue <- paste("&plcontinue=", xmlGetAttr(continue[[1]], "plcontinue"), sep=""))
    i <- i + 1
  }
  return(docs)
}


##' Extract Links from a Wikipedia file
##'
##' This functions extracts the links of an wikipedia article.
##' @title wiki links from xml
##' @param xml_files A vector containing plain character xml documents
##' @return A character vector with the links
##' @author chris
get_links <- function(xml_files){
  plyr::llply(xml_files, .fun = function(doc) {
    parsed_doc <- xmlParse(doc)
    ans <- getNodeSet(parsed_doc, "//*/pages/page//*/pl")
    rel.articles <- sapply(ans, xmlGetAttr, "title")
  })
}
