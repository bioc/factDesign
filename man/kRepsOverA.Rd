\name{kRepsOverA}
\alias{kRepsOverA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A filter function for at least k sets of replicates in a factorial experiment to have mean larger than A.}
\description{
`kRepsOverA' returns a filter function with bindings for `k' and `A'.  This function evalutes `TRUE' is at least `k' of the means of the replicates are larger than `A'.
}
\usage{
kRepsOverA(k, A = 100, INDEX)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{The number of sets of replicates with mean greater than A.}
  \item{A}{The value to exceed. }
  \item{INDEX}{List of factors, each of the same length as the input vector. }
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  A function with bindings for `A', `k', and `INDEX'.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Denise Scholtens }
%\note{ ~~further notes~~ }

%~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{kOverA}},\code{\link{pOverA}} }
\examples{

library(affy)
library(genefilter)
data(estrogen)
#select the replicates with values larger than 5
f1 <- kRepsOverA(1,5,INDEX=phenoData(estrogen))
genefilter(estrogen[1:30],f1)

}
\keyword{design }
\keyword{univar }
