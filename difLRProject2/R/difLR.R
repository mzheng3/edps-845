#' Differential Item Functioning Study
#'
#' Functions for estimating differential item functioning with a set of
#' scored item responses.The difLR is used to Calculate the "logistic regression"
#' likelihood-ratio statistics and effect sizes for DIF detection"Logistic" for logistic regression
#' (Swaminathan and Rogers,1990).
#'
#' @param x matrix or data.frame whose rows correspond to the subjects and columns to the items..
#' @param member is group indicates the column of Data which corresponds to the group membership, either by specifying its name or by giving the column
#' @param member.type either "group" (default) to specify that group membership is made of two groups,
#' or "cont" to indicate that group membership is based on a continuous criterion.
#' @param SCORES are the test score, or any continuous or discrete variable with the same length as the number of rows of Data
#' @param anchor is a vector of integer values specifying which items (all by default) are currently considered as anchor (DIF free) items.
#' @param type is a character string specifying which DIF effects must be tested. Possible values are "both" (default), "udif" and "nudif".
#' @param criterion a character string specifying which DIF statistic is computed. Possible values are "LRT" (default) or "Wald".
#' @param na.rm logical with default \code{FALSE} specifying whether missings
#' should be removed before calculating individual descriptives.
#' @param GROUP optional grouping variable.
#' @export

## The member.type argument is set to "group", with two distinct (numeric or factor) values,
## say 0 and 1 (for the reference and focal groups respectively). Those
## values are internally transformed onto factors to denote group memebership.
## The three possible models to be fitted are:
##  M0 : logit(pi_g) = alpha + beta*X + gamma_g + delta_g*X
##  M1 : logit(pi_g) = alpha + beta*X + gamma_g
##  M2 : logit(pi_g) = alpha + beta*X
##  where pi_g is the probability of answering correctly the item in group g and X is the
##  matching variable. Parameters alpha and beta are the intercept and the slope of the
##  logistic curves (common to all groups), while gamma_g and delta_g are group-specific
##  parameters. For identification reasons the parameters gamma_0 and alpha_0 for
##  reference group (g = 0) are set to zero. The parameter gamma_1 of the focal group (g = 1)
##  represents the uniform DIF effect, and the parameter delta_1 is used to model nonuniform DIF effect.
##  The models are fitted with the glm function.
difLR<-function (x, member, member.type="group",match="score", anchor = 1:ncol(x), type = "both",
    criterion = "LRT")
{
    np <- nrow(x)
    ni <- ncol(x)
    item <- x[, ncol(x)]
    R2 <- function(np, ni) 1 - (exp(-np$null.deviance/2 + np$deviance/2))^(2/ni)
    R2max <- function(np, ni) 1 - (exp(-np$null.deviance/2))^(2/ni)
    R2DIF <- function(np, ni) R2(np, ni)/R2max(np, ni)
    dev <- R2full<-R2simple<-deltaR <- NULL
    npFull <- npSimple <- matrix(0, ncol(x), 4)
    if (member.type=="group") GROUP<-as.factor(member)
else GROUP<-member
    for (item in 1:ncol(x)) {
if (match[1]=="score"){
        xl <- x[, anchor]
        if (sum(anchor == item) == 0)
            xl <- cbind(xl, x[, item])
        SCORES <- rowSums(xl, na.rm = TRUE)
      }
else SCORES<-match
	  ITEM<-x[,item]
        m0 <- switch(type, both = glm(ITEM ~SCORES * GROUP, family = "binomial"),
            udif = glm(ITEM ~ SCORES + GROUP, family = "binomial"),
            nudif = glm(ITEM ~ SCORES * GROUP, family = "binomial"))
        m1 <- switch(type, both = glm(ITEM ~ SCORES, family = "binomial"),
            udif = glm(ITEM ~ SCORES, family = "binomial"),
            nudif = glm(ITEM ~ SCORES + GROUP, family = "binomial"))
        if (criterion == "LRT")
            dev[item] <- deviance(m1) - deviance(m0)
        else {
            if (criterion != "Wald")
                stop("'criterion' must be either 'LRT' or Wald'",
                  call. = FALSE)
            else {
                coeff <- as.numeric(coefficients(m0))
                covMat <- summary(m0)$cov.scaled
                if (type == "udif")
                  C <- rbind(c(0, 0, 1))
                else {
                  if (type == "nudif")
                    C <- rbind(c(0, 0, 0, 1))
                  else C <- rbind(c(0, 0, 1, 0), c(0, 0, 0, 1))
                }
                dev[item] <- t(C %*% coeff) %*% solve(C %*% covMat %*%
                  t(C)) %*% C %*% coeff
            }
        }
        R2full[item] <- R2DIF(m0, nrow(x))
        R2simple[item] <- R2DIF(m1, nrow(x))
        deltaR[item] <- R2DIF(m0, nrow(x)) - R2DIF(m1, nrow(x))
        npFull[item, 1:length(m0$coefficients)] <- m0$coefficients
        npSimple[item, 1:length(m1$coefficients)] <- m1$coefficients
    }
    colnames(npFull) <- colnames(npSimple) <- c("(Intercept)",
        "SCORE", "GROUP", "SCORE:GROUP")
    res <- list(stat = dev, R2M0=R2full,R2M1=R2simple, deltaR2 = deltaR, parM0 = npFull,
        parM1 = npSimple, criterion = criterion,member.type=member.type,
match=ifelse(match[1]=="score","score","matching variable"))
    return(res)
}



