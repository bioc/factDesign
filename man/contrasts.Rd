\name{contrasts}
\alias{contrastTest}
\alias{par2lambda}


\title{Construct appropriate lambda matrix and test linear contrasts of the parameter estimates from a linear model. }

\description{

'par2lambda' takes list of lm coefficient names and a corresponding list of numeric vectors corresponding to hypothesis tests of linear contrasts and returns a lambda matrix suitable for an F-test of the linear contrasts.  'par2lambda' is intended to be used in conjunction with 'contrastTest' or 'findFC'.

'contrastTest' performs an F test for simultaneous tests of linear contrasts using an appropriately specified lambda matrix and an lm object.
}
\usage{
par2lambda(betaNames, betas, coefs)
contrastTest(model, lambda, cVec = NA, p = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{betaNames}{A character vector of the names of the coefficients in a linear model. }
  \item{betas}{A list of vectors of the parameters to be used in the contrasts. }
  \item{coefs}{A list of vectors of numeric coefficients corresponding to betas. }
  \item{model}{An lm object. }
  \item{lambda}{A matrix of coefficients in the appropriate order to be multiplied by the estimated coefficients of the lm object, possibly returned from 'par2lambda'. }
  \item{cVec}{A vector of constants for testing that the linear contrasts equal something other than zero.  If this is unspecified, it is assumed to be zero. }
  \item{p}{The significance level at which to perform the contrast test. }
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
For \code{par2lambda}: A lambda matrix constructed for testing linear contrasts using lm output.


For \code{contrastTest}:
  \item{test }{Returns "REJECT" or "FAIL TO REJECT" based on the result of the test of hypothesis.}
  \item{Fstat}{The F statistic for the test of contrast.}
  \item{pvalue}{The corresponding pvalue for the F test.}
  \item{cEst}{The contrast estimate.}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Denise Scholtens}
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{par2lambda}},\code{\link{lm}}  }
\examples{

data(estrogen)
ES <- pData(estrogen)[["ES"]]
TIME <- pData(estrogen)[["TIME"]]   
fit <- lm(exprs(estrogen)["40079_at",] ~ ES + TIME + ES*TIME)
betaNames <- names(fit[["coef"]])
betas <- list(c("ESP"),c("ESP","ESP:TIME48h"))
coefs <- list(c(1),c(1,1))
lambda <- par2lambda(betaNames,betas,coefs)
contrastTest(fit,lambda)




}
\keyword{models }
\keyword{design }
\keyword{htest }
