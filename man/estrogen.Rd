\name{estrogen}
\alias{estrogen}
\docType{data}
\title{Microarray Data from an Experiment on Breast Cancer Cells}
\description{
  Gene expression levels for 500 genes from a 2x2 factorial experiment on MCF7 breast cancer cells using Affymetrix HGU95av2 arrays.  
}
\usage{data(estrogen)}
\format{
  
 An exprSet with 500 genes, 8 samples, and 2 variables.

}
\details{
The factors in this experiment were estrogen (ES: P or A) and length of exposure (TIME: 10 or 48 hours).   Gene expression values were estimated using rma after quantile normalization (see the 'affy' package).  Expression estimates are reported log base 2, as suggested by the rma method.
}
\source{
  Scholtens et al.  Analyzing Factorial Designed Microarray Experiments.  Journal of Multivariate Analysis.  To appear. 
}


\examples{
data(estrogen)
pData(estrogen)
exprs(estrogen)[1,]
}
\keyword{datasets}
