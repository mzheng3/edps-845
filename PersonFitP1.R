#' @title Person Fit Statistics
#'
#' @description This Function estimates person fit of dichotomous data
#' with responses 0 or 1 using Rasch model.
#'
#'@details Drasgow et al. (1985) introduced one of the most used person-fit
#'statistics, lz. This statistic is the standardized log-likelihood of the
#'respondent’s response vector. lz is (supposed to be) asymptotically
#'standard normally distributed. The computation of lz requires that both item
#'and ability parameters are available. Function lz allows to user to enter
#'his/her own item and ability parameter estimates (variables IP and Ability,
#'respectively). Alternatively, lz relies on functions available through the irtoys
#'package for estimating the parameters. Specifically, the IRT model to fit the data
#'is used here is: IRT.PModel="1PL". As for estimating the ability parameters there are three
#'possible methods: Ability.PModel="ML" (maximum likelihood), Ability.PModel="BM" (Bayes modal), or Ability.PModel="WL" (weighted likelihood).
#'using the argument Ability.PModel.
#'Aberrant response behavior is (potentially) indicated by small values of lz
#'(i.e., in the left tail of the sampling distribution).Missing values in matrix
#'are dealt with by means of pairwise elimination by default. Alternatively,
#'single imputation is also available. Three single imputation methods exist:
#'Hotdeck imputation (NA.method = "Hotdeck"), nonparametric model imputation
#'(NA.method = "NPModel"), and parametric model imputation (NA.method = "PModel");
#'see Zhang and Walker (2008).
#'
#'
#'@param n required number of observations. If
#'\code{length(n) > 1}, the length is taken to be the number required.
#'@param v1, v2 two values from which random sample will be taken.
#'@examples
#'
#' # Read/Import dichotomous data.
#' data <- read.table ()
#'

#'
#'@export
#'
irtstudy <- function(x, complete = FALSE, ...) {

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
  ##  Convert the 'wide' format to 'long' using melt.
  library(reshape2)
  melt(x)

  #  Rasch model
  m <- lme4::glmer(score ~ -1 + (1 | item) + (1 | person),
                   data = xl, family = "binomial")
  dat <- data.frame(x, theta = unlist(lme4::ranef(m)$person))
  ip <- data.frame(a = 1, b = -unlist(lme4::ranef(m)$item), c = 0,
                   row.names = inames)
  dat$se <- rtef(ip, dat$theta)$se

  #new function for person fit
  ## PModel.imputation method to deal with the missing values.
  PModel.imputation <- function(x, save.matImp, ip, model, ability, method, mu, sigma)
  {
    N <- dim(x)[1]; I <- dim(x)[2]

    # Estimate item parameters if not provided (using 'irtoys'):
    ip <- estIP(x, ip, model)

    # Estimate ability parameters if not provided (using 'irtoys'):
    ability <- estAb(x, ip, ability, method, mu, sigma)

    A   <- ip[, 1]; B <- ip[, 2]; C <- ip[, 3] #A=Item discrimination, B=Item difficulty, C=Guessing parameter.
    P   <- do.call(cbind, lapply(1:I, function (x) {C[x] + (1 - C[x]) / (1 + exp(-A[x] * (ability - B[x])))}))
    #
    x.imp  <- x
    position.NA <- which(is.na(x) == 1, arr.ind = TRUE)
    P.NA        <- P[position.NA]
    x.imp[position.NA] <- rbinom(length(P.NA), 1, P.NA)
    #
    if (save.matImp == TRUE)
    {
      write.matrix(x.imp, file="~.txt", sep=" ")
    }
    return(list(x.imp, ip, ability, 1))
  }

  #person fit statistics, NA.method =PModel, IRT.PModel = "1PL"; Compute the
  #lz (Drasgow, Levine, and Williams, 1985) person-fit statistics.
  library(irtoys)
  library(ltm)
  library(mirt)

  lz(x,
     NA.method = "PModel", Save.MatImp = FALSE,
     IP = NULL, IRT.PModel = "1PL", Ability = NULL, Ability.PModel = "WL")
  lz.out <- lz (x)

  out <- list(data = dat, ip = ip, np = np, ni = ni,
              vc = unlist(lme4::VarCorr(m)),
              fit = c(AIC = AIC(m), BIC = BIC(m), logLik = logLik(m),
                      deviance = deviance(m), df.residual = df.residual(m)))
  class(out) <- c("irtstudy", "list")

  return(out)
}

m <- lme4::glmer(score ~ -1 + (1 | item) + (1 | person),
                 data = x, family = "binomial")

dat <- data.frame(x, theta = unlist(lme4::ranef(m)$person))
dat$se <- rtef(ip, dat$theta)$se #test error funtion.


library(epmr)
irtstudy(x)

#' @export
print.irtstudy <- function(x, ...) {
  cat("\nItem Response Theory Study\n\n")
  cat(x$np, "people,", x$ni, "items\n\n")
  cat("Model fit with lme4::glmer\n")
  print(x$fit)
  cat("\nRandom effects\n")
  print(data.frame(Std.Dev = sqrt(x$vc), Var = x$vc,
                   row.names = c("person", "item")))
}

# Item response function
#' @rdname irtstudy
#' @export
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

# Item information function
#' @rdname irtstudy
#' @export
riif <- function(ip, theta = seq(-4, 4, length = 100)){

  out <- rirf(ip, theta)
  out <- data.frame(theta = theta, out[, -1] * (1 - out[, -1]))
  colnames(out)[-1] <- rownames(ip)
  return(out)
}

# Item error function
#' @rdname irtstudy
#' @export
rief <- function(ip, theta = seq(-4, 4, length = 100)){

  out <- data.frame(theta = theta, 1 / sqrt(riif(ip, theta)[, -1]))
  colnames(out)[-1] <- rownames(ip)
  return(out)
}

# Test response function
#' @rdname irtstudy
#' @export
rtrf <- function(ip, theta = seq(-4, 4, length = 100)){

  out <- data.frame(theta = theta,
                    p = apply(rirf(ip, theta)[, -1], 1, sum))
  return(out)
}

# Test information function
#' @rdname irtstudy
#' @export
rtif <- function(ip, theta = seq(-4, 4, length = 100)){

  out <- data.frame(theta = theta,
                    i = apply(riif(ip, theta)[, -1], 1, sum))
  return(out)
}

# Test error function
#' @rdname irtstudy
#' @export
rtef <- function(ip, theta = seq(-4, 4, length = 100)){

  out <- data.frame(theta = theta,
                    se = 1 / sqrt(rtif(ip, theta)$i))
  return(out)
}

booklet1 <- read.delim("C:/Users/Zheng/Desktop/Summer 2016/edps-845/projects/booklet1.txt", header=FALSE)
irtstudy(booklet1)
lz(booklet1)
