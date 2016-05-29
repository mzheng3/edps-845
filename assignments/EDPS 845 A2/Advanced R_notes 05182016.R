temp <- c("a", 1, 2, "bla")
class(temp)
tempcbind(1:3, letters[1:3])
templm <- lm()
tempz <- scale(rnorm(10))
tempz <- scale(rnorm(10, 50, 10))
tempz
as.data.frame(temp)

tempdf$v3 <- as.integer(tempdf$v1)

library ("equate")
PISA
colnames(PISA$students)
names(PISA)
class(PISA$totals)
names(PISA$totals)
tempdat <-PISA$totals$b1

sapply (1:ncol(tempdat), function(j)
  mean(tempdat[,j]))
colMeans(tempdat)
dim(tempdat)
apply(tempdat,1, sum) # 1 is the first dimension of the data, row.
apply(tempdat,2, mean) # 2 is the second dimension of the data

tempsum <- apply(tempdat, 2, sum, na.rm = T)
rowSums(tempdat, na.rm = T)
tempdat[tempdat < 3] <- NA

lapply(PISA$totals, ncol)
lapply(PISA$totals, nrow)

PISA$students[1:5, 1:8]
tapply(PISA$students$m033q01, PISA$students$schoolid, mean, na.rm = T) # mo33q01 BY schoolid, means by schoolid

with(PISA$students, 
     {tapply(m033q01, list(schoolid, bookid), mean, 
             na.rm = T)})
install.packages("devtools")
install.packages("knitr")
install.packages('ggplot2')

devtools::install_github("talbano/epmr")

library("epmr")
library("ggplot2")