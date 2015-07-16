getPartialRUnconditional <-
function(indicator,
                 variable,
                 sampleData,
                 sampleDesign)
{ #++
    # Estimates unconditional partial R-indicators.
 
    stopifnot(variable %in% names(sampleData))

    categories  <- sampleData[[variable]]
    RBiasFactor <- indicator$RBiasFactor
    nPopulation <- sum(indicator$sampleDesign$weights)
    propMean    <- indicator$propMean          
 
    arg <- with(indicator,
            data.frame(
                    n    = sampleDesign$weights,
                    prop = sampleDesign$weights * prop))

    byCategory <- within(
            aggregate(arg, list(category = categories), sum), {
                    prop     <- prop / n
                    propSign <- sign(n * (prop - propMean))
                    propVar  <- n * (prop - propMean)^2 / nPopulation
                    PuUnadj  <- propSign * sqrt(propVar) })

    model <- within(indicator$model,
            formula <- replaceRHSByVariable(formula, variable))
 
    propVar <- sum(byCategory$propVar)
    Pu      <- sqrt(propVar * RBiasFactor)
    PuUnadj <- sqrt(propVar)
    PuSE    <- 0.5 * getRSampleBased(model, sampleData, sampleDesign)$RSE
 
    partialIndicator <- list(
            type       = 'Unconditional partial R-indicator, sample based',
            variable   = variable,
            Pu         = Pu,
            PuUnadj    = PuUnadj,
            PuSE       = PuSE,
            byCategory = byCategory)
 
    partialIndicator <- getVariancePartialRUnconditional(
            partialIndicator, indicator, sampleData, sampleDesign)
 
    partialIndicator$byCategory <-
            partialIndicator$byCategory[c('category', 'PuUnadj', 'PuUnadjSE')]

    return (partialIndicator)
}
