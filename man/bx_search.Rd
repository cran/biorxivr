% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/bx_search.R
\name{bx_search}
\alias{bx_search}
\title{Search biorxiv.org}
\usage{
bx_search(query, limit = 10)
}
\arguments{
\item{query}{the terms to search for in biorxiv}

\item{limit}{the number of results to return}
}
\value{
a list with the following elements: a vector of URL's for bioRxiv papers from the search terms,and the maximum number of results
}
\description{
return a list of URLS, IDS and number of results found from search of biorxiv.org
}
\details{
This uses the generic search interface, therefore be aware that you'll have to do post download filtering if you want terms in a specific field
}
\examples{
\dontrun{
  ### Get search results
  bxEco <- bx_search("ecology",limit=20)
  summary(bxEco)
}
}
