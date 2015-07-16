\name{getPartialRConditional}
\alias{getPartialRConditional}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getPartialRConditional(indicator, variable, sampleData, sampleDesign)
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
    sampleWeights <- indicator$sampleDesign$weights
    modelVariables <- getVariables(indicator$model$formula, FALSE)
    otherVariables <- modelVariables \%sub\% variable
    otherCategories <- as.list(sampleData[otherVariables])
    propMeanByOthers <- with(indicator, ave(sampleWeights * prop, 
        otherCategories, FUN = sum)/ave(sampleWeights, otherCategories, 
        FUN = sum))
    zMeanByOthers <- apply(indicator$z, 2, FUN = function(x) return(ave(sampleWeights * 
        x, otherCategories, FUN = sum)/ave(sampleWeights, otherCategories, 
        FUN = sum)))
    categories <- sampleData[[variable]]
    RBiasFactor <- indicator$RBiasFactor
    weights <- sampleWeights/sum(sampleWeights)
    arg <- with(indicator, data.frame(n = sampleWeights, propVar = weights * 
        (prop - propMeanByOthers)^2))
    byCategory <- within(aggregate(arg, list(category = categories), 
        sum), {
        PcUnadj <- sqrt(propVar)
    })
    propVar <- sum(byCategory$propVar)
    Pc <- sqrt(propVar * RBiasFactor)
    PcUnadj <- sqrt(propVar)
    partialIndicator <- list(type = "Conditional partial R-indicator, sample based", 
        variable = variable, Pc = Pc, PcUnadj = PcUnadj, byCategory = byCategory, 
        propMeanByOthers = propMeanByOthers, zMeanByOthers = zMeanByOthers)
    partialIndicator <- getVariancePartialRConditional(partialIndicator, 
        indicator, sampleData, sampleDesign)
    partialIndicator$byCategory <- partialIndicator$byCategory[c("category", 
        "PcUnadj", "PcUnadjSE")]
    return(partialIndicator)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
