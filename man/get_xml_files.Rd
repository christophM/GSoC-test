\name{get_xml_files}
\alias{get_xml_files}
\title{get_xml_files}
\usage{
  get_xml_files(title, prop = "links")
}
\arguments{
  \item{title}{A string with the title of the article}

  \item{prop}{The property of the article, which should be
  requested(e.g. links)}
}
\value{
  A vector containing plain character xml documents
}
\description{
  Download XML-file with links using the Wiki API
}
\details{
  This Function uses the Wikipedia API to download the
  properties (given by parameter prop) of an article. The
  resulting XML-files are limited in size by the API.
  Therefore multiple requests have to be made and the
  result is a list of XML documents.
}
\author{
  Christoph Molnar
}

