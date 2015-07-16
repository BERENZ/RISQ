\name{getPartialRUnconditional}
\alias{getPartialRUnconditional}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getPartialRUnconditional(indicator, variable, sampleData, sampleDesign)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{indicator}{
%%     ~~Describe \code{indicator} here~~
}
  \item{variable}{
%%     ~~Describe \code{variable} here~~
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
function (indicator, variable, sampleData, sampleDesign) 
{
    stopifnot(variable \%in\% names(sampleData))
    categories <- sampleData[[variable]]
    RBiasFactor <- indicator$RBiasFactor
    nPopulation <- sum(indicator$sampleDesign$weights)
    propMean <- indicator$propMean
    arg <- with(indicator, data.frame(n = sampleDesign$weights, 
        prop = sampleDesign$weights * prop))
    byCategory <- within(aggregate(arg, list(category = categories), 
        sum), {
        prop <- prop/n
        propSign <- sign(n * (prop - propMean))
        propVar <- n * (prop - propMean)^2/nPopulation
        PuUnadj <- propSign * sqrt(propVar)
    })
    model <- within(indicator$model, formula <- replaceRHSByVariable(formula, 
        variable))
    propVar <- sum(byCategory$propVar)
    Pu <- sqrt(propVar * RBiasFactor)
    PuUnadj <- sqrt(propVar)
    PuSE <- 0.5 * getRSampleBased(model, sampleData, sampleDesign)$RSE
    partialIndicator <- list(type = "Unconditional partial R-indicator, sample based", 
        variable = variable, Pu = Pu, PuUnadj = PuUnadj, PuSE = PuSE, 
        byCategory = byCategory)
    partialIndicator <- getVariancePartialRUnconditional(partialIndicator, 
        indicator, sampleData, sampleDesign)
    partialIndicator$byCategory <- partialIndicator$byCategory[c("category", 
        "PuUnadj", "PuUnadjSE")]
    return(partialIndicator)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
