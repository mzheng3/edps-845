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

##x = data, group =
difLR <- function(x, group, focal, complete = TRUE,
                  na.rm = FALSE){

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
  xl <- data.frame(score = c(unlist(x)), group = rep(1:np, ni), reference = rep(0, ni),
                   item = rep(1:ni, each = np), row.names = NULL)


  m1 <- lme4::glmer(score ~ -1 + (1 | item),
                   data = xl, family = "binomial")


  m2 <- lme4::glmer(score ~ -1 + (1 | item) + (1 | group),
                    data = xl, family = "binomial")


  m3 <- lme4::glmer(score ~ -1 + (1 | item) + (1 | group)* (1 | reference),
                    data = xl, family = "binomial")

  anova(m1, m2)
  anova(m2, m3)
  anova(m1, m3)

  out <- list(data = x, group = group, reference = reference,
              fit = c(AIC = AIC(m1),
                      BIC = BIC(m1),
                      logLik = logLik(m1),
                      deviance = deviance(m1)))


  class(out) <- c("difLR", "list")

  return(out)

}





#out <- list(data = xl, group = group, reference = reference,
            #fit = c(AIC1 = AIC(m1), AIC2 = AIC(m2), AIC3 = AIC(m3),
                    #BIC1 = BIC(m1), BIC2 = BIC(m2), BIC3 = BIC(m3),
                    #logLik1 = logLik(m1),logLik2 = logLik(m2),logLik3 = logLik(m3)
                    #deviance1 = deviance(m1), deviance2 = deviance(m2),deviance3 = deviance(m3)))

