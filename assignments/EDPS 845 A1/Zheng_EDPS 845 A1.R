#A1
# Zheng
# May 16, 2016
#----------------------------------------

library ("lme4")
data(Orthodont, package="nlme")
Orthodont$nsex<- as.numeric(Orthodont$Sex)
Orthodont$nsexage<-with(Orthodont. nsex)
temp<-lmer(distance ~ age + 
             (age |Subject)+(0 + nsexage | Subject), data=Orthodont)
  
save(temp, file ="users/talbano...")

names(attributes(temp))
class(temp)

load("user/talbano...")

x<-c(5, 6, 1, 2, 3, 4, 5)

mean (x)

#Function codes
zscore <-function(x) {
  out<- (x-mean(x))/sd(x)
   class(out) <- "zscore"
   return(out)
}


print.zscore<-function(x) {
  print.default (round(x, 3))
}


myzs<-zscore(x)


myname <- "Emily"

class(myname)
myname


ournames <- c("Emily", "Tony", "Katie")



source("users/talbano/...")


#--------------------------------
library()
source()
#--------------------------------
# the normal distribution
Normal {stats}

# Q4-1
?qt
qt ()

state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa",
           "qld", "vic", "nsw", "vic", "qld", "qld", "sa", "tas",
           "sa", "nt", "wa", "vic", "qld", "nsw", "nsw", "wa",
           "sa", "act", "nsw", "vic", "vic", "act")

statef <- factor(state)

table(statef)

incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56,
             61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46,
             59, 46, 58, 43)

incmeans <- tapply(incomes, statef, mean)

stderr <- function(x) sqrt(var(x)/length(x))

incster <- tapply(incomes, statef, stderr)

data.frame(i = incomes, s = statef)
ns <- table(statef)
cis <- data.frame(mean = incmeans, stderr =  incster, 
                  state = names(incmeans),
  lower = qt (.25, ns - 1), 
                  upper = qt(.25, ns - 1, lower.tail = FALSE))
library ("ggplot2")
ggplot(cis, aes(state, mean) + 
         geom_point() +
         geom_errorbar(aes(ymax = mean + stderr, ymin =)))
qt (.025, ns - 1)
qt (.025, ns - 1, lower.tail = FALSE )

qt

#Q5-5
nsnew <- ns
dim(nsnew) <- c(1,8)
nsnew

c(nsnew)

attributes(nsnew)

dim(nsnew) <- NULL
nsnew


colnames(cis)

cis$lower.Freq <- NULL

head(cis)

#Chapter 2

myvector <- c(2, 5, 6, 3, 8, 9)
names(myvector)

cis

head(cis)

myvector[]

cis[1:2, 1:5]

cis[c("act", "nsw"), c("state")]


cis[c("act", "nsw"), c("state", "stderr")]
cis$mean
cis$mean > 50

subset(cis, cis$mean > 50)
cis[cis$mean > 50 & cis$stderr < 1,]

subset(cis, cis$mean > 50, cis$stderr < 1)

subset(cis, mean > 50, cis$stderr < 1)

subset(cis, mean > 50 & cis$stderr < 1)

myarray <- rnorm(1000)
dim(myarray) <- c(10, 10, 10)
myarray[1, 1, 1:10]
myarray[1, 1, 1]


#Chapter9 if ()else and ifelse ()
temp <- cis$mean > 50
ifelse (temp == TRUE, 1, 0)
temp
ifelse(temp, 1, 0)
sum(temp)

ifelse(c(0, 0, 1), 0, 1)
table(temp)
cis
statef
table(statef)
statef == "sa"
sum(statef == "sa")

tempmiss <- c(1, 2, 3, NA, NA, 6, 7)
tempmiss

is.na(tempmiss)

sum(is.na(tempmiss))

library("epmr")
sumcomp(tempmiss)

temp
qt


lower.tail <- TRUE

incomes
statef

for(state in statef) {
  print(mean(incomes[statef == state]))
}
  
incmeans <- tapply(incomes, statef, mean)

# Get means using a for loop
for(i in unique(statef)) {
  cat(i, mean(incomes[statef == i], "\n"))
}

cat("Heading for my output\n\nSome output")

# Do the same with sapply
?sapply
sapply(levels(statef), nchar)
  
sapply(levels(statef), function(i) mean(incomes[statef == i]))

# get means using a for loop updated
states <- levels(statef)

mymeans <- numeric(length = length(states))

i <- "act"
names(mymeans) <- states
for(i in states) {
 mymeans[i] <- mean(incomes[statef == i])
}
mymeans


# Get means and se using a for loop updates2

mymse <- data.frame(matrix(nrow = length(states), 
                           ncol = 2), row.names = states)
colnames(mymse) <- c("m", "se")
for(i in states) {
  mymse$m[i, "m"] <- mean(incomes[statef == i])
  mymse$m[i, "se"] <- stderr(incomes[statef == i])
}


#Do the same with sapply Updates
t(sapply(levels(statef), function(i)
  c(m = mean(incomes[statef == i]),
    se = stdeff(incomes[statef == i]))))
mymse

#Notes for EDPS 845 -R 05/17/2016

rbind(mymse, t(sapply(levels(statef))))


"dv ~ iv1 + iv2 + iv3 + iv1:iv2 + iv2:iv3"

myform <- formula (dv ~ iv1 + iv2 + iv3 + iv1:iv2 + iv2:iv3)
myform

myform <- formula (dv ~ iv1*iv2)
myform

?lm
library ("equate")

tempdat <- KBneat$x
head(tempdat)

str(tempdat)

lm(total ~ 1 + anchor, data = tempdat)

out <- lm(total ~ 1 + anchor, data = tempdat)
out

fitted

?fitted
getS3method("fitted") #not working
?getS3method


coef(out)

methods("fitted") #works

?presmoothing
equate::presmoothing()


#Chapter12 plotting

range(tempdat[, 2])

plot(0:36, 0:12, type = "n")

plot (c(0:36), c(0: 12), type = "n")

points(tempdat[,1], tempdat[,2])
lines(tempdat[,1], tempdat[,2])
title(main = "my plot")

plot(tempdat[, 1], tempdat[, 2], main [])

barplot(mymeans)
dotchart(mymeans)
hist(tempdat[, 1])
plot(tempdat[, 1])
?plot
?par

sd
?sd

equate::  #have access to the package without typing in library ("equate")

 library ("psych") 

#ALbano tools to make developing R package Easier

install.packages("devtools")
devtools::install_github("talbano/epmr")

library ("devtools")

library ("epmr")
  
#Simulation study

class simulate(out)



library (ltm)


rdat <- sim.rasch(nvar = 5, n = 500, low = -3, high = 3, d = NULL,
                  a = 1, mu = 0, sd =1)

rdat
rout <- rasch(rdat$items, constraint = cbind(ncol(rdat$items) +1, 1))
rout
rtheta <- factor.scores(rout)

rtheta


#Dstudy.R
plot.study <- function(x, ...)plot(1:nrow(x), x$mean)

plot

#Simulation study of IRT
psych:: sim.rasch
psych:: ICC





