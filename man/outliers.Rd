\name{outliers}

\alias{madOutPair}
\alias{outlierPair}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Detect single outliers in experimental designs with only two replicates per treatment condition. }
\description{

These function detect pairs of observations with unexpectedly large differences compared to the rest of the data and determine if one of the pair is a single outlier using median absolute deviation criteria.  

}
\usage{
outlierPair(x, INDEX, p = 0.05, na.rm = TRUE)
madOutPair(x, whichPair, c = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector of observations. }
  \item{INDEX}{A list of factors, each the same length as x, used to indicate the replicate observations. }
  \item{p}{The significance level at which to perform the test. }
  \item{na.rm}{If TRUE, will remove missing values.}
  \item{whichPair}{A result of outlierPair, recording which pair has largest difference between replicate observations. }
  \item{c}{The number of median absolute deviations to be used as a cutoff for determining single outliers.  }
}
\details{
This outlier detection method is useful for small factorial designs in which the usual residuals from a linear model would have a large number of linear dependencies compared to the actual number of residuals.  The function first calculates n difference between 2n replicates (call these pure residuals), and then constructs an F-statistic: f=(large squared p.r.)/((sum of remaining squared p.r.'s)/(n-1)).  An p-value (adjusted for taking the largest of the p.r.'s) is calculated by n*Pr(F(1,n-1)>f). If f>=n-1, this p-value is exact, otherwise it is an upper bound.

Once pairs with significantly large differences are identified using outlierPair, madOutPair is applied.  If only one of the tagged replicates falls outside the range of (med(x)-c*mad(x),med(x)+c*mad(x)), the observation is designated the single outlier.


}
\value{

For \code{outlierPair}:

  \item{test}{Returns TRUE if an outlier pair is detected at the specified level of significance p.}
  \item{pval}{The actual value of n*Pr(F(1,n-1)>f).}
  \item{whichPair}{The index of the pair of observations with the largest difference.}

For \code{madOutPair}:

   The index of the single outlier observation, or "NA" if no single outliers are detected.


}
\references{Scholtens et al.  Analyzing Factorial Designed Microarray Experiments.  Journal of Multivariate Analysis.  To appear. }
\author{Denise Scholtens}
%\note{ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{madOutPair}} }
\examples{

data(estrogen)

op1 <- outlierPair(exprs(estrogen)["728_at",],INDEX=pData(estrogen),p=.05)
print(op1)
madOutPair(exprs(estrogen)["728_at",],op1[[3]])

op2 <- outlierPair(exprs(estrogen)["33379_at",],INDEX=pData(estrogen),p=.05)
print(op2)
madOutPair(exprs(estrogen)["33379_at",],op2[[3]])








}
\keyword{models }
\keyword{design }
