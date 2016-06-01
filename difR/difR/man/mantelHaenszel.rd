\name{mantelHaenszel}
\alias{mantelHaenszel}

\title{Mantel-Haenszel DIF statistic}

\description{
 Calculates Mantel-Haenszel statistics for DIF detection. 
 }

\usage{
mantelHaenszel(data, member, correct = TRUE, exact = FALSE,
  anchor = 1:ncol(data))
 }

\arguments{
 \item{data}{numeric: the data matrix (one row per subject, one column per item).}
 \item{member}{numeric: the vector of group membership with zero and one entries only. See \bold{Details}.}
 \item{correct}{logical: should the continuity correction be used? (default is \code{TRUE}).}
 \item{exact}{logical: should an exact test be computed? (default is \code{FALSE}).}
 \item{anchor}{a vector of integer values specifying which items (all by default) are currently considered as anchor
              (DIF free) items. See \bold{Details}.}
 }

\value{
 A list with several arguments:
 \item{resMH}{the vector of the Mantel-Haenszel DIF statistics (either asymptotic or exact).}
 \item{resAlpha}{the vector of the (asymptotic) Mantel-Haenszel estimates of the common odds ratios. Returned only if
                 \code{exact} is \code{FALSE}.}
 \item{varLambda}{the (asymptotic) variance of the \eqn{\lambda_{MH}} statistic. Returned only if \code{exact} is 
                 \code{FALSE}. See \bold{Details}.}
 \item{Pval}{the exact P-values of the MH test. Returned only if \code{exact} is \code{TRUE}.}
 }
 
\details{
 This command basically computes the Mantel-Haenszel (1959) statistic in the specific framework of differential item 
 functioning. It forms the basic command of \code{\link{difMH}} and is specifically designed for this call.
 
 The data are passed through the \code{data} argument, with one row per subject and one column per item. 

 Missing values are allowed for item responses (not for group membership) but must be coded as \code{NA} values. They 
 are discarded from sum-score computation.
  
 The vector of group membership, specified with \code{member} argument, must hold only zeros and ones, a value of zero 
 corresponding to the reference group and a value of one to the focal group.
 
 By default, the continuity correction factor -0.5 is used (Holland and Thayer, 1988). One can nevertheless remove it by 
 specifying \code{correct=FALSE}.

 By default, the asymptotic Mantel-Haenszel statistic is computed. However, the exact statistics and related P-values can
 be obtained by specifying the logical argument \code{exact} to \code{TRUE}. See Agresti (1990, 1992) for further 
 details about exact inference.

 Option \code{anchor} sets the items which are considered as anchor items for computing Mantel-Haenszel statistics. Items
 other than the anchor items and the tested item are discarded. \code{anchor} must hold integer values specifying the
 column numbers of the corresponding anchor items. It is primarily designed to perform item purification.
 
 In addition to the Mantel-Haenszel statistics to identify DIF items, \code{mantelHaenszel} computes the estimates of the
 common odds ratio \eqn{\alpha_{MH}} which are used for measuring the effect size of the items (Holland and Thayer, 1985,
 1988). They are returned in the \code{resAlpha} argument of the output list. Moreover, the logarithm of 
 \eqn{\alpha_{MH}}, say \eqn{\lambda_{MH}}, is asymptotically distributed and its variance is computed and returned into
 the \code{varLambda} argument. Note that this variance is the one proposed by Philips and Holland (1987), since it seems
 the most accurate expression for the variance of \eqn{\lambda_{MH}} (Penfield and Camilli, 2007).
}

\references{
 Agresti, A. (1990). \emph{Categorical data analysis}. New York: Wiley.

 Agresti, A. (1992). A survey of exact inference for contingency tables. \emph{Statistical Science, 7}, 131-177.

 Holland, P. W. and Thayer, D. T. (1985). An alternative definition of the ETS delta scale of item difficulty. 
 \emph{Research Report RR-85-43}. Princeton, NJ: Educational Testing Service.

 Holland, P. W. and Thayer, D. T. (1988). Differential item performance and the Mantel-Haenszel procedure. In H. Wainer
 and H. I. Braun (Ed.), \emph{Test validity}. Hillsdale, NJ: Lawrence Erlbaum Associates.
 
 Magis, D., Beland, S., Tuerlinckx, F. and De Boeck, P. (2010). A general framework and an R package for the detection
 of dichotomous differential item functioning. \emph{Behavior Research Methods, 42}, 847-862.

 Mantel, N. and Haenszel, W. (1959). Statistical aspects of the analysis of data from retrospective studies of disease. 
 \emph{Journal of the National Cancer Institute, 22}, 719-748.
 
 Penfield, R. D., and Camilli, G. (2007). Differential item functioning and item bias. In C. R. Rao and S. Sinharray 
 (Eds.), \emph{Handbook of Statistics 26: Psychometrics} (pp. 125-167). Amsterdam, The Netherlands: Elsevier.

 Philips, A., and Holland, P. W. (1987). Estimators of the Mantel-Haenszel log odds-ratio estimate. \emph{Biometrics, 43},
 425-431.
 }


\author{
    Sebastien Beland \cr
    Collectif pour le Developpement et les Applications en Mesure et Evaluation (Cdame) \cr
    Universite du Quebec a Montreal \cr
    \email{sebastien.beland.1@hotmail.com}, \url{http://www.cdame.uqam.ca/} \cr
    David Magis \cr
    Department of Education, University of Liege \cr
    Research Group of Quantitative Psychology and Individual Differences, KU Leuven \cr
    \email{David.Magis@ulg.ac.be}, \url{http://ppw.kuleuven.be/okp/home/} \cr
    Gilles Raiche \cr
    Collectif pour le Developpement et les Applications en Mesure et Evaluation (Cdame) \cr
    Universite du Quebec a Montreal \cr
    \email{raiche.gilles@uqam.ca}, \url{http://www.cdame.uqam.ca/} \cr 
 }


\seealso{
 \code{\link{difMH}}, \code{\link{dichoDif}}
 }

\examples{
\dontrun{

 # Loading of the verbal data
 data(verbal)

 # With and without continuity correction
 mantelHaenszel(verbal[,1:24], verbal[,26])
 mantelHaenszel(verbal[,1:24], verbal[,26], correct = FALSE)
 
 # Exact test
 mantelHaenszel(verbal[,1:24], verbal[,26], exact = TRUE)

 # Removing item 6 from the set of anchor items
 mantelHaenszel(verbal[,1:24], verbal[,26], anchor = c(1:5,7:24))
 }
 }



