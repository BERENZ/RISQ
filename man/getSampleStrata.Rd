\name{getSampleStrata}
\alias{getSampleStrata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getSampleStrata(sampleWeights, nMaxStrata = 20)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sampleWeights}{
%%     ~~Describe \code{sampleWeights} here~~
}
  \item{nMaxStrata}{
%%     ~~Describe \code{nMaxStrata} here~~
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
function (sampleWeights, nMaxStrata = 20) 
{
    weights <- sort(unique(sampleWeights))
    indices <- seq(along = sampleWeights)
    strata <- factor(seq(along = weights))
    if (length(weights) <= nMaxStrata) {
        values <- merge(data.frame(weight = sampleWeights, index = indices), 
            data.frame(weight = weights, stratum = strata), all.x = TRUE)
        sampleStrata <- values[order(values$index), "stratum"]
    }
    else sampleStrata <- factor(rep(1, length(sampleWeights)))
    return(sampleStrata)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
