#' @title Simulate Dichotomous Data
#'
#' @description This Function simulates dichotomous data.
#'
#'@details Here are some details about our cool function
#'This function was developed in front of the EDPS-845 class.
#'A few of them tried to type these things as we wrote this section.
#'
#'@param n required number of observations. If
#'\code{length(n) > 1}, the length is taken to be the number required.
#'@param v1, v2 two values from which random sample will be taken.
#'@examples
#'
#' # Generate dichotomous data for 10 people
#' dsim(10)
#'
#' # Show that dsim is comparable to round(runif())
#' mean(sapply(1:1000, function(i)
#' mean(dsim(100))))
#' mean(sapply(1:1000, function(i)
#' mean(round)))

#'
#'@export
dsim <- function(n, v1 = 0, v2 = 1) {
  if(!is.numeric(n))
    stop("'n' must be numeric, obviously.")
  if(length(n) > 1)
    n <- length(n)
  out <- sample (0:1, n, replace = TRUE)
  return(out)

}

dsim(100)
round(runif(10))
sample(c(0, 1), 10, replace = T)

?lme4
??rpat
?rpat
?irtoys
library(devtools)
setwd("C:/Users/Zheng/Desktop/Summer 2016/dif")
getwd()
devtools::create_description()
devtools::document()

?unlist
list(booklet1)
unlist(booklet1) # to produce a vector of all atomic components in a matrix (here booklet1),
#i.e., a single long vector of matrix booklet1.

help("!")
help("paste0")
