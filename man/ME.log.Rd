\name{ME.log}
\alias{ME.log}

\title{Provides Reliable LFDR Estimates by Selecting an Appropriate Reference Class}
\description{
%Based on Section 2.2, Algorithm 1, and Section 5.2.
%section 3: "BME" Bayes estimator of ME estimates of LFDR
Selects an appropriate reference class given two reference classes. Considers two vecotr of LFDR estimates computed based on the two alternative reference classes and provides a vector of more reliable LFDR estimates.
}
\usage{
ME.log(stat,lfdr.C,p0.C,ncp.C,p0.S,ncp.S,a=3,lower.p0=0,upper.p0=1,
lower.ncp=0.1,upper.ncp=50,length.p0=200,length.ncp=200)
}

\arguments{
  \item{stat}{A vector of test statistics for SNPs falling inside the
  intersection of the separate and combined reference classes.}
  \item{lfdr.C}{A data frame of local false discovery rates of features falling
  inside the intersection of the separate and combined reference classes,
  computed based on all features belonging to the combined reference class.}
  \item{p0.C}{An estimate of the proportion of the non-associated features
  applied to the combined reference class. }
  \item{ncp.C}{A non-centrality parameter applied to the combined reference
  class.}
  \item{p0.S}{An estimate of the proportion of the non-associated features
  applied to the separate reference class. }
  \item{ncp.S}{A non-centrality parameter applied to the separate reference
  class.}
  \item{a}{Parameter used to define the grade of evidence that alternative
  reference class should be favoured instead of the separate reference class.}
  \item{lower.p0}{The lower bound for the proportion of unassociated features.}
  \item{upper.p0}{The upper bound for the proportion of unassociated features.}
  \item{lower.ncp}{The lower bound for the non-centrality parameter.}
  \item{upper.ncp}{The lower bound for the non-centrality parameter.}
  \item{length.p0}{Desired length of a sequence vector containing the proportion
  of non-associated features. The sequences starts from \code{lower.p0} and ends
  at \code{upper.p0}.}
  \item{length.ncp}{Desired length of a sequence vector containing
  non-centrality parameters. The sequences starts from \code{lower.ncp} and ends
  up at \code{upper.ncp}.}
}

\details{The terms `separate' and `combined' reference classes are used when one
sample population (reference class) is a subset of the other. Detailed
explanations can be found in the vignette
"Using the LFDREmpiricalBayes Package".

}

\value{
Returns the following values:

\item{p0.hat}{estimate of the proportion of non-associated SNPs}
\item{ncp.hat}{estimate of the non-centrality parameter}
\item{LFDR.hat}{A vector of LFDR estimates for features falling inside the
intersection of the separate and combined reference classes, obtained by the
Maximum Entropy method.}
}

\note{
 The vector of test statistics: \code{stat}, need to be positive values in order
 for the function \code{ME.log} to work.
}

\references{
Karimnezhad, A. and Bickel, D. R. (2016). Incorporating prior knowledge about
genetic variants into the analysis of genetic association data: An empirical
Bayes approach. Working paper. Retrieved from
\url{http://hdl.handle.net/10393/34889}
}
\author{
  Code: Ali Karimnezhad. \cr
  Documentation: Johnary Kim and Anna Akpawu. \cr
}

\examples{
#import the function ``lfdr.mle'' from package``LFDR.MLE''
library(LFDR.MLE)

#Consider a separate reference class and a combined reference class below:

n.SNPs.S<-3  # number of SNPs in the separate reference class
n.SNPs.Sc<-2 # number of SNPs in the complement of the separate reference class.

#Create a series of test statistics for SNPs in the separate reference class.
stat.Small<-rchisq(n.SNPs.S,df=1,ncp=0)
ncp.Sc<-10

#Create a series of test statistics for SNPs in the combined reference class.
stat.Big<-c(stat.Small,rchisq(n.SNPs.Sc,df=1,ncp=ncp.Sc))

#Using lfdr.mle, a series of arguments are used.
dFUN=dchisq; lower.ncp = .1; upper.ncp = 50;
lower.p0 = 0; upper.p0 = 1;

#Maximum Likelihood estimates for the LFDRs of SNPs in the created
# separate reference class.

#Separate reference class.
estimates.S<-lfdr.mle(x=stat.Small,dFUN=dchisq,df=1,lower.ncp = lower.ncp,
upper.ncp = upper.ncp)
LFDR.Small<-estimates.S$LFDR
p0.Small<-estimates.S$p0.hat
ncp.Small<-estimates.S$ncp.hat

# Maximum Likelihood estimates for the LFDRs of SNPs in the created combined
# reference class.
estimates.C<-lfdr.mle(x=stat.Big,dFUN=dchisq,df=1,lower.ncp = lower.ncp,
upper.ncp = upper.ncp)
LFDR.Big<-estimates.C$LFDR
p0.Big<-estimates.C$p0.hat
ncp.Big<-estimates.C$ncp.hat


#The first three values of the combined reference class correspond to the
#separate reference class in this example
LFDR.SBig<-LFDR.Big[1:3]

LFDR.ME<-ME.log(stat=stat.Small,lfdr.C=LFDR.SBig,p0.C=p0.Big,ncp.C=ncp.Big,
p0.S=p0.Small,ncp.S=ncp.Small)

LFDR.ME
}

\keyword{Maximum Entropy Method}
\keyword{LFDR Estimation}
\keyword{Maximum Likelihood}
\keyword{Reference Class}
\keyword{Separate Reference Class}
\keyword{Combined Reference Class}
\keyword{Maximum Likelihood Estimation}
