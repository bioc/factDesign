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
The factors in this experiment were estrogen (ES: P or A) and length of exposure (TIME: 10 or 48 hours).   Gene expression values were estimated using the PM-only method in dChip after normalization using the Invariant Set Method (Li and Wong, 2001).
}
\source{
  Scholtens et al.  Analyzing Factorial Designed Microarray Experiments.  Journal of Multivariate Analysis.  To appear. 
}
\references{
  Li and Wong (2001).  Model-based analysis of oligonucleotide arrays: Expression index computation and outlier detection.  PNAS. 98:31-36.
}
\examples{
data(estrogen)
pData(estrogen)
plot(exprs(estrogen)[1,])
}
\keyword{datasets}
