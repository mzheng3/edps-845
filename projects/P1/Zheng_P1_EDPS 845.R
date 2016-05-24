#############################################################################
# SIMULATED EXAMPLE 2: Simulated data according to the Rasch testlet model
#############################################################################

set.seed(678)
N <- 3000   # number of persons
I <- 4      # number of items per testlet
TT <- 3     # number of testlets

ITT <- I*TT
b <- round( stats::rnorm( ITT , mean=0 , sd = 1 ) , 2 )
sd0 <- 1 # sd trait
sdt <- seq( 0 , 2 , len=TT ) # sd testlets
sdt <- sdt
# simulate theta
theta <- stats::rnorm( N , sd = sd0 )
# simulate testlets
ut <- matrix(0,nrow=N , ncol=TT )
for (tt in 1:TT){
  ut[,tt] <- stats::rnorm( N , sd = sdt[tt] )
}
ut <- ut[ , rep(1:TT,each=I) ]
# calculate response probability
prob <- matrix( stats::pnorm( theta + ut + matrix( b , nrow=N , ncol=ITT ,
                                                   byrow=TRUE ) ) , N, ITT)
Y <- (matrix( stats::runif(N*ITT) , N , ITT) < prob )*1
colMeans(Y)
# define testlets
testlets <- rep(1:TT , each=I )
burnin <- 300
iter <- 1000