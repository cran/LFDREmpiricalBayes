\name{SEL.caution.parameter}
\alias{SEL.caution.parameter}

\title{Based on a Decision-Theoretic Approach, Performs a Multiple Hyothesis Testing Problem under an Squared Error Loss Function}

\description{
Assuming a squared error loss function, it provides three caution-type actions using estimated LFDRs computed based on both separate and combined reference classes. }

\usage{
SEL.caution.parameter(x1,x2)
}

\arguments{
  \item{x1}{Input numeric vector of LFDR estimates in the separate reference
  class.}
  \item{x2}{Input numeric vector of LFDR estimates in the combined reference
  class.}
}

\value{
Much like \code{caution.parameter.actions}, this function returns three vectors
of equal size as seen below:
 \item{CGM1}{Squared error loss value for the Conditional Gamma Minimax
 (CGMinimax).}
 \item{CGM0}{Squared error loss value for the Conditional Gamma Minimin
 (CGMinimin).}
 \item{CGM0.5}{Squared error loss value for the Action/Decision estimate
 (a balance between CGMinimax and CGMinimin.}

For each index of the vectors, the squared error loss values are given.
}

\references{
Karimnezhad, A. and Bickel, D. R. (2016). Incorporating prior knowledge about
genetic variants into the analysis of genetic association data: An empirical
Bayes approach. Working paper. Retrieved from
\url{http://hdl.handle.net/10393/34889}
}

\examples{

#Similar to caution.parameter actions we have the following classes

#First reference class
LFDR.Separate <- c(0.14, 0.8, 0.16, 0.30)
#Second reference class
LFDR.Combined <- c(0.21, 0.61, 0.12, 0.10)

output <- SEL.caution.parameter(LFDR.Separate, LFDR.Combined)

# Three caution cases with SEL values.
output
}

\author{
  Code: Ali Karimnezhad. \cr
  Documentation: Johnary Kim and Anna Akpawu. \cr
}

\keyword{Squared Error Loss}
\keyword{Error}
\keyword{Hypothesis Indicator}
