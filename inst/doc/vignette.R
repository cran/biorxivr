## ----searching----------------------------------------------------------------
library(biorxivr)
bxEco <- bx_search("Ecology",limit = 10)
summary(bxEco)

## ----hidden,echo=F------------------------------------------------------------
bxEco <- bx_search("Gotelli and Hart",limit = 10)

## ----get details--------------------------------------------------------------
bxEcoDetails <-  bx_extract(bxEco)

bxEcoDetails[[1]]


## ----dl,eval=FALSE------------------------------------------------------------
#  
#  bx_download(bxEco,"~/biorxiv_pdfs")
#  

