\name{LFDREmpiricalBayes-package}
\alias{LFDREmpiricalBayes-package}
\alias{LFDREmpiricalBayes}
\docType{package}

\title{
\packageTitle{LFDREmpiricalBayes}
}

\description{
\packageDescription{LFDREmpiricalBayes}
}

\details{
\tabular{ll}{
Package: \tab LFDREmpiricalBayes\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2017-09-26\cr
License: \tab GPL-3\cr
Depends: \tab R(>= 2.14.2) \cr
Imports: \tab matrixStats, stats\cr
Suggests: \tab LFDR.MLE \cr
URL: \tab https://davidbickel.com\cr
}

%The package LFDREmpiricalBayes contains a series of functions aiming at analyzing the association of single nucleotide polymorphisms (SNPs) to some particular disease. The functions use estimated local false discovery rates (LFDRs) of SNPs within a sample population that we define as a "reference class". Of the proposed methods, the maximum entropy method is based on ratio of two likelihood functions generated on the basis of two alternative reference classes. The other methods are based on robust Bayes approaches and are applicable to more than two reference classes. The explanations in this report allow for a better understanding of the basic ideas of how the functions in this package work as well as detailed examples that are useful in analyzing a given data set. Although SNPs are used throughout this document, other biological data such as protein data and other gene data can be used.

%\packageDESCRIPTION{LFDREmpiricalBayes}
%\packageIndices{LFDREmpiricalBayes}
%%~~ An overview of how to use the package, including the most important ~~
%%~~ functions ~~
}
\author{
\packageAuthor{LFDREmpiricalBayes}

Maintainer: \packageMaintainer{LFDREmpiricalBayes}
}
\references{
Karimnezhad, A. and Bickel, D. R. (2016). Incorporating prior knowledge about
genetic variants into the analysis of genetic association data: An empirical
Bayes approach. Working paper. Retrieved from
\url{http://hdl.handle.net/10393/34889}}

\keyword{ package }
\seealso{
  For more information on how to interpret the outputs, look at the
  supplementary file in the vignette directory,
  "Using the LFDREmpiricalBayes Package."
}

