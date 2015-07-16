\name{getVariancePartialRConditional}
\alias{getVariancePartialRConditional}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getVariancePartialRConditional(partialIndicator, indicator, sampleData, sampleDesign)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{partialIndicator}{
%%     ~~Describe \code{partialIndicator} here~~
}
  \item{indicator}{
%%     ~~Describe \code{indicator} here~~
}
  \item{sampleData}{
%%     ~~Describe \code{sampleData} here~~
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
function (partialIndicator, indicator, sampleData, sampleDesign) 
{
    byCategory <- partialIndicator$byCategory
    variable <- partialIndicator$variable
    sigma <- indicator$sigma
    weights <- sampleDesign$weights
    nPopulation <- sum(weights)
    propDelta <- indicator$prop - partialIndicator$propMeanByOthers
    zDelta <- indicator$z - partialIndicator$zMeanByOthers
    variance <- numeric()
    for (index in seq(nrow(byCategory))) {
        label <- byCategory[index, "category"]
        delta <- ifelse(sampleData[[variable]] == label, 1, 0)
        zDeltaWeight <- zDelta * delta * weights
        propDelta2 <- propDelta * propDelta * delta
        A <- matrix(propDelta, nrow = 1) \%*\% zDeltaWeight
        B <- t(zDelta) \%*\% zDeltaWeight
        V1 <- 4 * A \%*\% sigma \%*\% t(A)
        V2 <- 2 * getTrace(B \%*\% sigma \%*\% B \%*\% sigma)
        V3 <- sampleDesign$getVarTotal(sampleDesign, propDelta2)
        variance[index] <- 0.25 * (V1 + V2 + V3)/(nPopulation * 
            sum(propDelta2 * weights))
    }
    partialIndicator$byCategory <- within(byCategory, PcUnadjSE <- sqrt(variance))
    return(partialIndicator)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
