# Example code from class May 17, 2016

library("psych")
library("ltm")

state <- c("tas", "sa",  "qld", "nsw", "nsw", "nt",  "wa",  "wa",
  "qld", "vic", "nsw", "vic", "qld", "qld", "sa",  "tas",
  "sa",  "nt",  "wa",  "vic", "qld", "nsw", "nsw", "wa",
  "sa",  "act", "nsw", "vic", "vic", "act")
statef <- factor(state)
table(statef)
incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56,
  61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46,
  59, 46, 58, 43)
incmeans <- tapply(incomes, statef, mean)
stderr <- function(x) sqrt(var(x)/length(x))
incster <- tapply(incomes, statef, stderr)
ns <- table(statef)
cis <- data.frame(mean = incmeans, stderr = incster,
  state = names(incmeans),
  lower = qt(.025, ns - 1),
  upper = qt(.025, ns - 1, lower.tail = FALSE))

library("ggplot2")
ggplot(cis, aes(state, mean)) +
  geom_point() +
  geom_errorbar(aes(ymax = mean + stderr, ymin = mean - stderr))

# Get means using a for loop
states <- levels(statef)
mymeans <- numeric(length = length(states))
names(mymeans) <- states
for(i in states) {
  mymeans[i] <- mean(incomes[statef == i])
}

# Get means and se using a for loop
mymse <- data.frame(matrix(nrow = length(states),
  ncol = 2), row.names = states)
colnames(mymse) <- c("m", "se")
for(i in states) {
  mymse[i, "m"] <- mean(incomes[statef == i])
  mymse[i, "se"] <- stderr(incomes[statef == i])
}

# Do the same with sapply
t(sapply(levels(statef), function(i)
  c(m = mean(incomes[statef == i]),
  se = stderr(incomes[statef == i]))))

# Simulating data and fitting with rasch model
rdat <- sim.rasch(nvar = 5, n = 500, low = -3, high = 3,
  d = NULL, a = 1, mu = 0, sd = 1)
rout <- rasch(rdat$items,
  constraint = cbind(ncol(rdat$items) + 1, 1))
rtheta <- factor.scores(rout)
