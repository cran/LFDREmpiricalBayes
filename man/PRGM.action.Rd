\name{PRGM.action}
\alias{PRGM.action}

\title{Based on the Robust Bayes Approach, Performs a Multiple Hyothesis Testing Problem under an Squared Error Loss Function}

\description{
Assuming a squared error loss function, it provides Robust Bayes estimates of the LFDR estimates giving credit to both separate and combined reference classes.
}
\usage{
PRGM.action(x1,x2)
}

\arguments{
    \item{x1}{Input numeric vector of LFDR estimates of the separate reference
    class.}
    \item{x2}{Input numeric vector of LFDR estimated of the combined reference
    class.}
}

\value{
The output is a vector of the LFDR estimates based on the two reference classes.
%%based on SEL estimates.
}
\references{
Karimnezhad, A. and Bickel, D. R. (2016). Incorporating prior knowledge about
genetic variants into the analysis of genetic association data: An empirical
Bayes approach. Working paper. Retrieved from
\url{http://hdl.handle.net/10393/34889}
}

\examples{
#LFDR reference class values generated

#First reference class
LFDR.Separate <- c(0.14, 0.8, 0.16, 0.30)
#Second reference class
LFDR.Combined <- c(0.21, 0.61, 0.12, 0.10)

output <- PRGM.action(LFDR.Separate, LFDR.Combined)

# Vector of the LFDR estimates
output

}

\author{
  Code: Ali Karimnezhad. \cr
  Documentation: Johnary Kim and Anna Akpawu. \cr
}


\keyword{Posterior Regret Gamma Minimax}% use one of  RShowDoc("KEYWORDS")
\keyword{Average}% __ONLY ONE__ keyword per line
