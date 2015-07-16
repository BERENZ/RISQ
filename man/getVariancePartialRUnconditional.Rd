\name{getVariancePartialRUnconditional}
\alias{getVariancePartialRUnconditional}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getVariancePartialRUnconditional(partialIndicator, indicator, sampleData, sampleDesign)
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
    nPopulation <- sum(sampleDesign$weights)
    variable <- partialIndicator$variable
    byCategory <- partialIndicator$byCategory
    prop <- indicator$prop
    V1 <- numeric()
    V2 <- numeric()
    nSample <- nrow(sampleData)
    for (index in seq(nrow(byCategory))) {
        label <- byCategory[index, "category"]
        delta <- ifelse(sampleData[[variable]] == label, 1, 0)
        deltaC <- 1 - delta
        V1[index] <- sampleDesign$getVarTotal(sampleDesign, delta * 
            prop)
        V2[index] <- sampleDesign$getVarTotal(sampleDesign, deltaC * 
            prop)
    }
    partialIndicator$byCategory <- within(byCategory, {
        PuUnadjSE <- sqrt(n/nPopulation * (V1 * (1/n - 1/nPopulation)^2 + 
            V2 * (1/nPopulation)^2))
    })
    return(partialIndicator)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
