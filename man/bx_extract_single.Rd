\name{bx_extract_single}
\alias{bx_extract_single}
\title{Extract data from a single record}
\usage{
bx_extract_single(bxso_url)
}
\arguments{
\item{bxso_url}{The URL of a single biorxiv paper, so one element of the search results from bx_search().}
}
\value{
The output is an object of class biorxiv_paper. This object is a list where each item in the list is one of the papers found using bx_search(). Each of these items is itself a list with 2 items: authors and paper.   
\item{author}{This item is again a list with two items. The first item is called 'names' and it is a vector of characters, where each element is the name and last name of one of the authors. The second item is called 'emails' and it is a character vector, where each element of this vector is the email address of one of the authors.}
\item{paper}{This item is again a list with 5 items. The first item is called 'title' and it is a character vector of one element. The second item is called 'abstract' and it is a character vector of one element containing the whole abstract. The third item is called 'date' and it is a character vector of one element. The fourth item is called 'DOI' and it is a character vector of one element. The firth item is called 'fulltext_url' and it is a character vector of one element containing the full URL of the pdf.}
}
\description{
Generate an S3 object that represents a single paper
}
\examples{
bxEco <- bx_search("ecology",limit=1)
bxEcoData <- bx_extract_single(bxEco[[1]])
}

