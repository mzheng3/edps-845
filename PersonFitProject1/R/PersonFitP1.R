#' Item Response Theory Study
#'
#' Functions for estimating person fit.
#' Currently, only the 1 parameter Rasch model is supported.
#'
#' @param x matrix or data.frame of scored item responses, one row per person,
#' one column per item.
#' @param complete logical with default \code{FALSE} indicating whether or not
#' \code{x} should be reduced to rows with complete data across all columns.
#' @param ip vector or matrix of item parameters, with a in the first column, b
#' in the second, and c in the third.
#' @param theta vector of thetas over which to estimate the corresponding function.
#' @param ... further arguments passed to or from other functions, including
#' \code{\link{glmer}}
#' @export
Personfit <- function(x, complete = FALSE, ...) {

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
                          (1 + exp(ip[i, 1] * (-theta + ip[i, 2])))))
    colnames(out) <- rownames(ip)
    out <- data.frame(theta = theta, out)
    return(out)
  }


  pip <- rirf(ip, data$theta)[, -1] #p_j.
  lnL <- rowSums(rpat * log(pip) + (1 - rpat) * log(1 - pip))
  sumlnL <- rowSums(pip * log(pip) + (1-pip) * log(1 - pip))
  varlnL <- rowSums(pip * (1-pip) * (sqrt(log(pip/(1 - pip)))))

  lz <- as.vector(round(lnL-sumlnL)/sqrt(varlnL), 4)
  dat$se <- rtef(ip, dat$theta)$se

  out <- list(data = dat, ip = ip, np = np, ni = ni,lz = lz,
              vc = unlist(lme4::VarCorr(m)),
              fit = c(AIC = AIC(m), BIC = BIC(m), logLik = logLik(m),
                      deviance = deviance(m), df.residual = df.residual(m)))
  class(out) <- c("personfit", "list")

}

#Test personfit function
personfit <- function(x) {
  attach(x)
  rpat <- data[,ni]
  pip <- rirf(ip, data$theta)[, -1]
  lnL <- rowSums(rpat * log(pip) + (1 - rpat) * log(1 - pip))
  sumlnL <- rowSums(pip * log(pip) + (1-pip) * log(1 - pip))
  varlnL <- rowSums(pip * (1-pip) * (sqrt(log(pip/(1 - pip)))))
  lz <- as.vector(round(lnL-sumlnL)/sqrt(varlnL), 4)
}

