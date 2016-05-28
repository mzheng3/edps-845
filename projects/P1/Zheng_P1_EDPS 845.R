#Zheng P1 EDPS 845
library(knitr)
```{r}


```
?"knitr"

library(equate)
library(knitr)
test1 <- read.csv("test1.csv", row.names = 1) 
setwd("C:/Users/Zheng/Desktop/Summer 2016/EDPS 845 R/edps-845/in-class/")

(f = system.file(test1, "knitr-minimal.Rnw", package = "knitr"))
knit(f)  # compile to tex

purl(f)  # tangle R code
purl(f, documentation = 0)  # extract R code only
purl(f, documentation = 2)  # also include documentation


```{r}
#Rewrite the loop replacing tapply with our own function
dstudy2 <- function(thetest){
  out <- matrix(NA, nrow = length(levels(thetest$schoolid)), ncol = 9)

  for(j in 1:4){
    out[j, ] <- unlist(tapply(thetest[,j], thetest$schoolid, epmr::dstudy))
  
  }
  return(out)
}

# Rewrite without the for loop
dstudy3 <- function(thetest, whatcols, another = "blabla") {
  apply(thetest[, whatcols], 2, dstudy)
} # another = "blabla", ... to add/define a different function within a function.

#test it
lapply(test, function(x) apply(x[, 1:4], 2, dstudy)) # lappy = list of apply
lapply(test$test1, dstudy2)
lapply(test$test1, dstudy3)

```