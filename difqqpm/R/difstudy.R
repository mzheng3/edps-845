#' Differential Item Functioning Study
#'
#' Functions for estimating differential item functioning with a set of
#' scored item responses.
#'
#' @param x matrix or data.frame of scored item responses.
#' @param subset optional vector for selecting a subset of columns from \code{x}.
#' @param scores optional vector of construct scores.
#' @param complete logical with default \code{TRUE} indicating whether or not
#' \code{x} should be reduced to rows with complete data across all columns.
#' @param na.rm logical with default \code{FALSE} specifying whether missings
#' should be removed before calculating individual descriptives.
#' @param groups optional grouping variable.
#' @export
difLogisticR <- function(x, subset = 1:ncol(x), scores, complete = TRUE,
  na.rm = FALSE) {
  if(!all(unlist(x) %in% c(0, 1, NA)))
    stop("'x' can only contain score values 0, 1, and NA.")
  ni <- ncol(x)
  if(complete)
    x <- x[complete.cases, ]
  else {
    allna <- rowSums(is.na(x)) == ni
    if(any(allna)) {
      x <- x[!allna, ]
      warning(sum(allna), " cases with missing data on all items removed.")
    }
  }

  np <- nrow(x)
  inames <- colnames(x)
  if(is.null(inames))
    inames <- paste0("item", 1:ni)
  xl <- data.frame(score = c(unlist(x)), person = rep(1:np, ni),
                   item = rep(1:ni, each = np), row.names = NULL)

  m <- lme4::glmer(score ~ -1 + (1 | item) + (1 | person),
                   data = xl, family = "binomial")
  dat <- data.frame(x, theta = unlist(lme4::ranef(m)$person))
  ip <- data.frame(a = 1, b = -unlist(lme4::ranef(m)$item), c = 0,
                   row.names = inames)

  rpat <- data[, ni] #x_ij, person i's response to item j

  rirf <- function(ip, theta = seq(-4, 4, length = 100)){

    if(NCOL(ip) == 1)
      ip <- cbind(1, ip, 0)
    else if(NCOL(ip) == 2)
      ip <- cbind(ip, 0)
    if(NCOL(ip) != 3)
      stop("'ip' can only contain up to three parameters per item.")
    ni <- NROW(ip)
    out <- rbind(sapply(1:ni, function(i) ip[i, 3] + (1 - ip[i, 3]) /
                          (1 + exp(ip[i, 1] * (-theta + ip[i, 2]))))) #3PL
    colnames(out) <- rownames(ip)
    out <- data.frame(theta = theta, out)
    return(out)
  }

##1PL logistic model, llf=logl likelihood function, pg. 143, 6.8;
  #x=x_ij; pr = 1PL irt model; p = item parameter;
  # x=responses, p=parm list, theta=ability, mu, sigma
  llfRasch <- function(x, theta,p,mu,sigma,method) {
    pr <- 1.0/(1.0 + exp((p[,2] - theta))) ##See p18  2.8, de ayala.
    pr <- pmax(pr, .00001); pr <- pmin(pr, .99999)
    ni <- ncol(x)
    x <- data[,ni] # responses
    ll <- x*log(pr) + (1-x)*log(1.0-pr) #formula 6.8, p.143;
    lf <- sum(ll)
    if (method != "ML") lf <- lf + log(dnorm(x,mu,sigma))
    return(lf)
  }
}

 ## 3PL
  llf3PL <- function(x, theta,p,mu,sigma,method) {
    pr <- p[,3] + (1.0 - p[,3])/(1.0 + exp(p[,1]*(p[,2] - theta)))
    pr <- pmax(pr, .00001); pr <- pmin(pr, .99999)
    ll <- x*log(pr) + (1-x)*log(1.0-pr)
    lf <- sum(ll)
    if (method != "ML") lf <- lf + log(dnorm(x,mu,sigma))
    return(lf)
  }

  ##LR , Logtistic regression for the reference group

  LR <- function(xr, thetar,pr,mu,sigma,method) {
    prr <- 1.0/(1.0 + exp((p[,2] - thetar)))
    prr <- pmax(prr, .00001); prr <- pmin(prr, .99999)
    llr <- xr*log(prr) + (1-xr)*log(1.0-prr)
    lfr <- sum(llr)
    if (method != "ML") lfr <- lfr + log(dnorm(xr,mu,sigma))
    return(lfr)
  }
  ##LF, logistic regression for the focal group

  LF <- function(xf, thetaf,pf,mu,sigma,method) {
    prf <- 1.0/(1.0 + exp((p[,2] - thetaf)))
    prf <- pmax(prf, .00001); prf <- pmin(prf, .99999)
    llf <- xf*log(prf) + (1-xf)*log(1.0-prf)
    lff <- sum(llf)
    if (method != "ML") lff <- lff + log(dnorm(xf,mu,sigma))
    return(lff)
  }


  ##Delta G^2 P. 332, 12.4, De Ayala.

  deltaGsquared =
    (-2)(log(LR/LF)) = (-2)log(LR) - (-2)log(LF) = (-2)(log(LR)-log(LF))
    return(deltaGsquared)

