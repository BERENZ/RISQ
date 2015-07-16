\name{getRSampleBased}
\alias{getRSampleBased}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getRSampleBased(model, sampleData, sampleDesign)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
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
function (model, sampleData, sampleDesign) 
{
    sampleData <- within(sampleData, {
        sampleWeights <- sampleDesign$weights
        sampleWeights <- sampleWeights/mean(sampleWeights)
    })
    modelfit <- glm(model$formula, model$family, sampleData, 
        sampleWeights)
    prop <- predict(modelfit, type = "response")
    propMean <- weighted.mean(prop, sampleDesign$weights)
    propVar <- weightedVar(prop, sampleDesign$weights)
    sigma <- vcov(modelfit)
    x <- model.matrix(model$formula, sampleData)[, colnames(sigma)]
    z <- model$grad(predict(modelfit, type = "link")) * x
    withBiasAndVar <- !is.null(sigma) && all(!is.na(sigma))
    if (withBiasAndVar) {
        RBias <- getBiasRSampleBased(prop, z, sigma, sampleDesign)
        RVar <- getVarianceRSampleBased(prop, z, sigma, sampleDesign)
        if (RBias > propVar) 
            RBiasFactor <- 0
        else RBiasFactor <- 1 - RBias/propVar
    }
    else {
        RBias <- NA
        RBiasFactor <- NA
        RVar <- NA
    }
    CV <- sqrt(propVar)/propMean
    CVVar <- 0.25 * RVar/propMean^2 + CV^4/nrow(sampleData)
    indicator <- list(type = "R-indicator, sample based", sampleDesign = sampleDesign, 
        prop = prop, propMean = propMean, model = model, modelfit = modelfit, 
        sigma = sigma, z = z, R = 1 - 2 * sqrt(propVar * RBiasFactor), 
        RUnadj = 1 - 2 * sqrt(propVar), RSE = sqrt(RVar), RBiasFactor = RBiasFactor, 
        CV = CV, CVSE = sqrt(CVVar))
    return(indicator)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
