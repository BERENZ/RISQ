\name{getRIndicator}
\alias{getRIndicator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getRIndicator(formula, sampleData, sampleWeights = rep(1, nrow(sampleData)), sampleStrata = NULL, family = c("binomial", "gaussian"), withPartials = TRUE, otherVariables = character())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{sampleData}{
%%     ~~Describe \code{sampleData} here~~
}
  \item{sampleWeights}{
%%     ~~Describe \code{sampleWeights} here~~
}
  \item{sampleStrata}{
%%     ~~Describe \code{sampleStrata} here~~
}
  \item{family}{
%%     ~~Describe \code{family} here~~
}
  \item{withPartials}{
%%     ~~Describe \code{withPartials} here~~
}
  \item{otherVariables}{
%%     ~~Describe \code{otherVariables} here~~
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
function (formula, sampleData, sampleWeights = rep(1, nrow(sampleData)), 
    sampleStrata = NULL, family = c("binomial", "gaussian"), 
    withPartials = TRUE, otherVariables = character()) 
{
    nSample = nrow(sampleData)
    stopifnot(length(sampleWeights) == nSample)
    stopifnot(is.numeric(sampleWeights))
    if (is.null(sampleStrata)) 
        sampleStrata <- getSampleStrata(sampleWeights)
    stopifnot(is.factor(sampleStrata))
    stopifnot(length(sampleStrata) == nSample)
    sampleDesign <- getSampleDesign(sampleWeights, sampleStrata)
    family <- match.arg(family)
    model <- switch(family, binomial = list(formula = formula, 
        grad = function(mu) exp(mu)/(1 + exp(mu))^2, family = binomial(link = "logit")), 
        gaussian = list(formula = formula, grad = function(mu) 1, 
            family = gaussian(link = "identity")))
    indicator <- getRSampleBased(model, sampleData, sampleDesign)
    if (withPartials) 
        indicator$partials <- getPartialRs(indicator, sampleData, 
            sampleDesign, otherVariables)
    return(indicator)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
