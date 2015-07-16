\name{getPartialRs}
\alias{getPartialRs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getPartialRs(indicator, sampleData, sampleDesign, otherVariables = character())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{indicator}{
%%     ~~Describe \code{indicator} here~~
}
  \item{sampleData}{
%%     ~~Describe \code{sampleData} here~~
}
  \item{sampleDesign}{
%%     ~~Describe \code{sampleDesign} here~~
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
function (indicator, sampleData, sampleDesign, otherVariables = character()) 
{
    modelVariables <- getVariables(indicator$model$formula, FALSE)
    variables <- unique(c(modelVariables, otherVariables))
    byVariables <- NULL
    byCategories <- list()
    for (variable in variables) {
        pConditional <- getPartialRConditional(indicator, variable, 
            sampleData, sampleDesign)
        pUnconditional <- getPartialRUnconditional(indicator, 
            variable, sampleData, sampleDesign)
        byVariable <- data.frame(variable = variable, Pu = pUnconditional$Pu, 
            PuUnadj = pUnconditional$PuUnadj, PuSE = pUnconditional$PuSE, 
            Pc = pConditional$Pc, PcUnadj = pConditional$PcUnadj, 
            PcSEApprox = pUnconditional$PuSE)
        byVariables <- rbind(byVariables, byVariable)
        byCategory <- merge(pUnconditional$byCategory, pConditional$byCategory)
        byCategories <- c(byCategories, list(byCategory))
    }
    names(byCategories) <- byVariables$variable
    partialRs <- list(byVariables = byVariables, byCategories = byCategories)
    return(partialRs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
