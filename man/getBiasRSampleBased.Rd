\name{getBiasRSampleBased}
\alias{getBiasRSampleBased}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getBiasRSampleBased(prop, z, sigma, sampleDesign)
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
    nPopulation <- sum(sampleDesign$weights)
    propVar <- sampleDesign$getVarTotal(sampleDesign, prop)
    z <- z * sqrt(sampleDesign$weights)
    lambda1 <- sum(apply(z, 1, function(zi) return(t(zi) \%*\% 
        sigma \%*\% zi)))
    lambda2 <- propVar/nPopulation
    bias <- (lambda1 - lambda2)/nPopulation
    return(bias)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
