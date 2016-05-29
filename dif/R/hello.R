# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}
library(devtools)
install.packages("roxygen2")
setwd("C:/Users/Zheng/Desktop/Summer 2016/dif")
getwd("C:/Users/Zheng/Desktop/Summer 2016/dif")
getwd()
devtools::create_description()
devtools::document()


wlz <- function (x, complete = FALSE, ...) {
  NA.method = "PModel", Save = FALSE,
  Item = NULL, IRT.PModel = "1PL",                              Ability = NULL, Ability.PModel = "WL",
  mu = 0, sigma = 1)
}