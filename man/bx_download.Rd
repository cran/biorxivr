\name{bx_download}

\alias{bx_download}

\title{Download PDFs of papers found by bx_search}

\usage{
bx_download(bxso, directory, create = TRUE)
}

\arguments{
\item{bxso}{search results from bx_search()}

\item{directory}{The location you want to download the PDFs to.}

\item{create}{TRUE or FALSE. If true create the directory if it does not exist. Default is TRUE. }
}

\description{
Download PDFs of all the papers in your search results.
}

\value{
The PDFs are downloaded into the directory selected. Nothing is returned to R. 
}

\examples{
\donttest{
bxEco <- bx_search("ecology",limit=1)
bx_download(bxEco,tempdir())
}
}

