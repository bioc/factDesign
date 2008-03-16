\name{findFC}
\alias{findFC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to find the fold change between two experimental conditions in a factorial experiment based on the linear model parameter estimates.}
\description{
'findFC' constructs a point estimate of fold change using the linear model coefficients in an lm object.
}
\usage{
findFC(model, lambdaNum, lambdaDenom, logbase=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{An lm object.}
  \item{lambdaNum}{A numeric vector of coefficients for the parameters to be used in the numerator of the fold change estimate.}
  \item{lambdaDenom}{A numeric vector of coefficients for the parameters to be used in the denominator of the fold change estimate.}
  \item{logbase}{By default, set to NULL.  For log-transformed data, the base of the logarithm.  Specify "exp" for natural log-transformed data.}
}
\details{
\code{logbase}=NULL if the data have not been log-transformed.  The fold change estimate is calculated as the ratio for the parameter estimates corresponding to the experimental conditions of interest. 

\code{logbase}="exp" if the data have been natural log-transformed.  The fold change is calculated as the difference in the parameter estimates for the two conditions of interest, then exponentiated using exp().

\code{logbase} can be set to any number, for example 2, for other log transforms.  The fold change is calculated as the difference in the parameter estimates for the two conditions of interest, then exponentiated with \code{logbase} as the base.
}
\value{
  A point estimate of the fold change between the experimental conditions specified in the lambdaNum and lambdaDenom vectors.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Denise Scholtens}
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{par2lambda}}}
\examples{

data(estrogen)
ES <- pData(estrogen)[["ES"]]
TIME <- pData(estrogen)[["TIME"]]   
fit <- lm(exprs(estrogen)["33744_at",] ~ ES + TIME + ES*TIME)
betaNames <- names(coef(fit))
betas <- list(c("(Intercept)","ESP","TIME48h","ESP:TIME48h"),
					c("(Intercept)","ESP"))
coefs <- list(c(1,1,1,1),c(1,1))
lambda <- par2lambda(betaNames,betas,coefs)
findFC(fit,lambda[1,],lambda[2,],logbase=2)

}
\keyword{models}
\keyword{design}
