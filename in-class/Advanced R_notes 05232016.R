library(pryr)
df <- data.frame(x = 1:10, y = letters[1:10])
otype(df) 
otype(df$x)
otype(df$y)
mean
ftype(mean)


library(eRm)
RSM
eRm:::likLR