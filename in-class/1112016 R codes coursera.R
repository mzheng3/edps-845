x<-data.frame(foo = 1:4, bar = c (T, T, F, F))
x
nrow(x)
ncol(x)
x<-1:3
x<-c(1,2,3)
x
x
x<-c(1,2,3,4)
x
x<-c(1:44)
x
names(x)
names (x)<-c("foo", "bar", "norf")
names (x)
x<-1:3
names (x)<-c("foo", "bar", "norf")
names(x)
x
names(x)
x<-list(a=1, b=2, c=3)
x
m<-matrix(1:4, nrow = 2, ncol=2)
m
m<-matrix(1:4, nrow = 2, ncol=2)
dimnames(m)<-list(c("a", "b"), c("c", "d"))
m
x<-vector ("numeric", length=10)
y<-c(1.7, "a") ##character
x
data<-read.table ("foo.txt")
initial <-read.table ("datatable.txt", nrows=100)
classes<-sapply (initial, class)
tabALL<-read.table("datatable.txt", colClasses = classes)
install.packages("dumping")
y<-data.frame(a=1, b="a")
y
dput(y)
dput(y, file="y.R")
new.y<-dget("y.R")
new.y
str(file)
con<-file("foo.txt", "r")
data<-read.csv(con)
data<-read.csv("foo.txt")
con<-gzfile ("words.gz")
x<-readLines(con,10)
con<-gzfile ("words.gz")
con
x<-readLines(con,10)
x
con<-url("http://www.jhsph.edu", "r")
con<-url("http://www.jhsph.edu", "r")
x<-readLines(con)
head(x)
x<-c("a", "b", "c", "d", "e")
x<-c("a", "b", "c","c", "d", "a")
x
x[1]
x[2]
x[1]
x[2]
x[1:4]
x[x>"a"]
u<-x>"a"
u
x[u]
x<-list(foo = 1:4, bar=0.6)
x[1]
x[[1]]
x$bar
x[["bar"]]
x["bar"]
x<-list(foo=1:4, bar=0.6, baz="hello")
x[c(1, 3)]
x<-list(foo=1:4, bar=0.6, baz="hello")
x[c(1, 3)]
x<-list(foo=1:4, bar=0.6, baz="hello")
name<-"foo"
x[[name]]
x$name
x$foo
x<-list(a=list(10, 12, 14), b=c(3.14, 2.81))
x[[c(1,3)]]
x[[1]][[3]]
x[[1]][[3]]
x[[c(2, 1)]]
x<-matrix(1:6, 2, 3)
x[1,2]
x[2,1]
x[2,1]
x[2,1]
x[2,1]
x[2,1]
x<-matrix(1:6, 2, 3)
x
x<-[1,]
x<-matrix(1:6, 2, 3)
x[1,]
x[,2]
x<-matrix(1:6, 2, 3)
x[1,2]
x[1,2, drop=FALSE]
x<-matrix(1:6, 2, 3)
x[1,]
x[1,, drop=FALSE]
x<-list(aardvark=1:5)
x
x$a
x[["a"]]
x[["a", exact=FALSE]]
x<-c(1,2, NA, 4, NA, 5)
bad<-is.na(x)
x[!bad]
y<-c("a", "b", NA, "d", NA, "f")
y
good<-complete.cases(x,y)
good
x[good]
y[good]
airquality[1:6,]
good<-complete.cases(airquality)
good
airquality[good,][1:6]
x<-1:4; y<-6:9
x+y
x>2
x>=2
y==8
x*Y
x*y
x/y
x<-matrix(1:4, 2,2); y<-matrix(rep(10,4), 2, 2)
x
y
x*y
x/y
x%*%y
X<-4L
X
X<-c(4, "a", TRUE)
X
X<-c(1, 3, 5); Y<-c(3, 2, 10)
cbind.data.frame(X, Y)
cbind(X, Y)
x<-list(2, "a", "b", TRUE)
x[[2]]
x<-1:4
y<-2:3
x+y
x<-1:4
x
x[x>10]==4
x<-c(17, 14, 4, 5, 13, 12, 10)
x[x==4]>10
data<-hw1_data.csv
read.csv ("hw1_data.csv")
setwd("C:/Users/Zheng/Desktop/1. R Programming")
setwd("C:/Users/Zheng/Desktop/1. R Programming")
setwd("C:/Users/Zheng/Desktop/1. R Programming")
read.csv ("hw1_data.csv")
x<-c("Ozone")
mean(x)
summary(x)
x<-list(2, "a", "b", TRUE)
x[[2]]
x<-list(2, "a", "b", TRUE)
x[[1]]
x<-1:4
y<-2
x+y
x<-c(3, 5, 1, 10, 12, 6)
x<-c(3, 5, 1, 10, 12, 6)
x[x<6]==0
x[x==6]<-0
x[x==6]
x[x<6]<-0
x[x<6]
x[x<=5]<-0
x[x<=5]
x[x%in%1:5]<-0
x[x%in%1:5]
x[x==0]<-6
x[x==0]
x<-c(4, TRUE)
x
x<-c(1,3,5); y<-c(3, 2, 10); rbind(x, y)
if (x>3) {
y <-10
} else {
y <- 0
}
y<-if (x>3) {
10
} else {
0
}
for (i in 1:10) {
print(i)
}
x<-c("a", "b", "c", "d")
x
for (i in 1:4) {
print(i)
}
for(i in seq_along(x) {
print(x[i])
}
for(i in seq_along(x){
print(x[i])
}
for(i in seq_along(x)){
print(x[i])
}
for (letter in x) {
print(x[i])
}
for (i in 1:4) print(x[i])
x<-matrix(1:6, 2, 3)
x
x
for (i in seq_len(nrow(x))) {
for(j in seq_len(ncol(x))) {
print(x[i,j])
}
}
while (count <-10) {
print(count)
count <-count +1
}
count<-0
while (count <-10) {
print(count)
count <-count +1
}
z<-5
while (z >= 3 && z<= 10) {
print (z)
coin <-rbincm [1,1,0.5]
if(coin == 1) { ## random walk
z<- z+1
} else {
z<-z-1
}
}
z<-5
while (z >= 3 && z<= 10) {
print (z)
coin <-rbinom [1,1,0.5]
if(coin == 1) { ## random walk
z<- z+1
} else {
z<-z-1
}
}
z<-5
while (z >= 3 && z<= 10) {
print (z)
coin <-rbinom [1,1,0.5]
if(coin == 1) { ## random walk
z<- z+1
} else {
z<-z-1
}
}
z<-5
while (z >= 3 && z<= 10) {
print (z)
coin <- rbinom [1,1,0.5]
if(coin == 1) { ## random walk
z<- z + 1
} else {
z<- z - 1
}
}
while(z >= 3 && z<= 10) {
print (z)
coin <- rbinom [1,1,0.5]
if(coin == 1) { ## random walk
z<- z + 1
} else {
z<- z - 1
}
}
z<-5
while(z >= 3 && z<= 10) {
print (z)
coin <- rbinom (1, 1, 0.5)
if(coin == 1) { ## random walk
z<- z + 1
} else {
z<- z - 1
}
}
x0 <-1
tol<-le-8
x0 <-1
tol<-1e-8
repeat {
x1 <-computeEstimate()
if(abs(x1 - x0) < tol) {
break
} else {
x0 <- x1
}
}
for (i in 1:100) {
if (i <- 20) {
## Skip the first 20 iterations
next
}
## Do something here
}
add2<-function(x, y) {
x + y
}
add2(3, 5)
above10 <- function(x) {
use <- x > 10
x[use]
}
above <- function (x, n) {
use <- x > n
x[use]
}
x<- 1:20
above (x, 12)
columnmean <- function (y) {
nc <- ncol(y)
means <- numeric (nc)
for (i in 1:nc) {
mean [i] <- mean (y[,i])
}
means
}
columnmean(airquality)
read.csv("mydata.csv")
getwd()
load("C:/Users/Zheng/Desktop/1. R Programming/hw1_data.csv")
getwd()
read.csv ("my_sim.csv")
setwd("C:/Users/Zheng/Desktop/1. R Programming")
read.csv ("my_sim.csv")
read.csv("mydata.csv")
read.csv ("hw1_data.csv")
columnmean("hw1_data.csv")
columnmean(hw1_data.csv)
columnmean <- function (y) {
nc <- ncol(y)
means <- numeric (nc)
for (i in 1:nc) {
mean [i] <- mean (y[,i])
}
means
}
columnmean(airquality)
help ("airquality")
read ("airquality")
airquality
columnmean(airquality)
summary(airquality)
columnmean <- function (y) {
nc <- ncol(y)
means <- numeric (nc)
for (i in 1:nc) {
mean [i] <- mean (y[,i])
}
means
}
columnmean(airquality)
columnmean <- function (y) {
nc <- ncol(y)
means <- numeric(nc)
for (i in 1:nc) {
mean [i] <- mean (y[,i])
}
means
}
mean(airquality)
columnmean <- function (y, removeNA = TRUE) {
nc <- ncol(y)
means <- numeric(nc)
for (i in 1:nc) {
mean [i] <- mean (y[,i])
}
means
}
columnmean(airquality)
columnmean <- function (y, removeNA = TRUE) {
nc <- ncol(y)
means <- numeric(nc)
for (i in 1:nc) {
mean [i] <- mean (y[,i], na.rm = removeNA)
}
means
}
columnmean(airquality)
columnmean <- function (y, removeNA = TRUE) {
nc <- ncol(y)
means <- numeric(nc)
for (i in 1:nc) {
mean [i] <- mean (y[,i], na.rm = removeNA)
}
means
}
columnmean(airquality) ## not working
columnmean <- function (y) {
nc <- ncol(y)
means <- numeric(nc)
for (i in 1:nc) {
mean [i] <- mean (y[,i])
}
means
}
columnmean(airquality) ## not working
summary(airquality)
columnmean <- function (y) {
nc <- ncol(y)
means <- numeric(nc)
for (i in 1:nc) {
mean [i] <- mean (y[,i])
}
means
}
columnmean1 <- function (y) {
nc <- ncol(y)
means <- numeric(nc)
for (i in 1:nc) {
mean [i] <- mean (y[,i])
}
means
}
columnmean1(airquality)
columnmean2 <- function (y, removeNA = TRUE) {
nc <- ncol(y)
means <- numeric(nc)
for (i in 1:nc) {
mean [i] <- mean (y[,i], na.rm = removeNA)
}
means
}
columnmean2(airquality) ## not working
savehistory("C:/Users/Zheng/Desktop/1. R Programming/1112016 R codes coursera.R")
