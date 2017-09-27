\name{caution.parameter.actions}
\alias{caution.parameter.actions}

\title{Based on a Decision-Theoretic Approach, Performs a Multiple Hypothesis Testing Problem under a Zero-One Loss Function}

\description{Assuming a zero-onr loss function, it provides three caution-type actions using estimated LFDRs computed based on both separate and combined reference classes.
}

\usage{
caution.parameter.actions(x1,x2,l1=4,l2=1) # default values l1=4 and l2=1
# to obtain a threshold of 20\%.
}

\arguments{
  \item{x1}{A vector of LFDRs in the combined reference class.}
  \item{x2}{A vector of LFDRs in the separate reference class.}
  \item{l1}{Loss value (Type-I error) for deriving the threshold of the Bayes
  action.}
  \item{l2}{Loss value (Type-II error) for deriving the threshold of the Bayes
  action.}
}

\details{
%Based on Section 4.1 of Ali's paper
Accepts previously obtained LFDR estimates of SNPs falling inside the intersection of the separate and combined reference classes. The LFDR estimates of some biological feature (SNP or gene) within a sample population that we will refer to as `reference class'. If a reference class, containing LFDR estimates
is a subset of the other, it is referred to as `separate class'.
The entire set of LFDR estimates is called a `combined' reference class. Then,
a multiple hypothesis problem is conducted using three caution-type estimators.
The threshold set for rejecting the null hypothesis is derived from
pre-specified \code{l1} and \code{l2} values. Since having a type-I error is
worse than a type-II error, \code{l1} is recommende to be greater than
\code{l2}.

In generating the output, there are two potential outputs for each index of the
three caution-type actions. Check the \bold{Value} section for the
corresponding caution-type actions.

For each index of the output, one of two potential outputs based on Bayes action
are shown:
\tabular{rll}{
 \code{0} \tab Do not reject the null hypothesis \cr
 \code{1} \tab Reject the null hypothesis\cr
}
For each corresponding index in the output, the decision on whether to reject or
not reject the null hypothesis for biological feature can be based on
\code{CGM1}, \code{CGM0}, and \code{CGM0.5} decisions. Check \bold{See Also} for
more details on how to better interpret the outputs.

}

\value{

Outputs three vectors of equal size as seen below:
 \item{CGM1}{Decision values for the Conditional Gamma Minimax (CGMinimax).}
 \item{CGM0}{Decision values for the Conditional Gamma Minimin (CGMinimin).}
 \item{CGM0.5}{Decision values for the CG0.5 caution case (a balance between
 CGMinimax and CGMinimin.}

Note that the length of the input vectors \code{x1} and \code{x2} determines the
number of null hypothesis values seen in the output.
}

\note{
A limitation to the code is that both reference classes: \code{x1} and \code{x2}
must be of the same vector length.
}

\references{
Karimnezhad, A. and Bickel, D. R. (2016). Incorporating prior knowledge about
genetic variants into the analysis of genetic association data: An empirical
Bayes approach. Working paper. Retrieved from
\url{http://hdl.handle.net/10393/34889}
}

\author{
  Code: Ali Karimnezhad. \cr
  Documentation: Justin Chitpin, Anna Akpawu and Johnary Kim. \cr
}

\seealso{
  For more information on how to interpret the outputs, look at the vignette,
  ``Using \code{LFDREmpiricalBayes}''.
}

\examples{
#LFDR reference class values generated

#First reference class (separate class)
LFDR.Separate <- c(.14,.8,.251,.30)
#Second reference class (combined class)
LFDR.Combined <- c(.21,.61,.0888,.10)

# Default threshold at (20\%).
output <- caution.parameter.actions(x1=LFDR.Separate, x2=LFDR.Combined)

# Three caution cases
output

}

\keyword{ Local False Discovery Rate }
\keyword{ Caution Gamma Minimax }
\keyword{ Caution Gamma Minimin }
\keyword{ Caution-Type Estimators }
\keyword{ Null Hypothesis }
\keyword{ Bayes Action }
\keyword{ Threshold }
\keyword{ Loss Values }
\keyword{ Decision-Theoretic Method }
