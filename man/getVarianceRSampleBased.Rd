\name{getVarianceRSampleBased}
\alias{getVarianceRSampleBased}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getVarianceRSampleBased(prop, z, sigma, sampleDesign)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prop}{
%%     ~~Describe \code{prop} here~~
}
  \item{z}{
%%     ~~Describe \code{z} here~~
}
  \item{sigma}{
%%     ~~Describe \code{sigma} here~~
}
  \item{sampleDesign}{
%%     ~~Describe \code{sampleDesign} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (prop, z, sigma, sampleDesign) 
{
    weights <- sampleDesign$weights
    nSample <- length(weights)
    nPopulation <- sum(weights)
    propMean <- weighted.mean(prop, weights)
    propVar <- weightedVar(prop, weights, method = "ML")
    propZ <- cbind(prop, z)
    A <- cov.wt(propZ, wt = weights, method = "ML")$cov[-1, 1]
    B <- cov.wt(z, wt = weights, method = "ML")$cov
    C <- sampleDesign$getVarTotal(sampleDesign, (prop - propMean)^2)
    variance <- numeric()
    variance[1] <- 4 * t(A) \%*\% sigma \%*\% A
    variance[2] <- 2 * getTrace(B \%*\% sigma \%*\% B \%*\% sigma)
    variance[3] <- C/nPopulation^2
    variance <- sum(variance)/propVar
    return(variance)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
