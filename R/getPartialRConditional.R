getPartialRConditional <-
function(indicator,
                 variable,
                 sampleData,
                 sampleDesign)
{ #++
    # Estimates conditional partial R-indicators.
 
    stopifnot(variable %in% names(sampleData))
 
    sampleWeights <- indicator$sampleDesign$weights
    modelVariables  <- getVariables(indicator$model$formula, FALSE)
    otherVariables  <- modelVariables %sub% variable
    otherCategories <- as.list(sampleData[otherVariables])

    propMeanByOthers <- with(indicator,
            ave(sampleWeights * prop, otherCategories, FUN = sum) /
            ave(sampleWeights, otherCategories, FUN = sum))

    zMeanByOthers <- apply(
            indicator$z, 2,
            FUN = function(x) return (
                    ave(sampleWeights * x, otherCategories, FUN = sum) /
                    ave(sampleWeights, otherCategories, FUN = sum)))

    categories  <- sampleData[[variable]]
    RBiasFactor <- indicator$RBiasFactor
    weights     <- sampleWeights / sum(sampleWeights)
    
    arg <- with(indicator,
            data.frame(
                    n       = sampleWeights,
                    propVar = weights * (prop - propMeanByOthers)^2))
    
    byCategory <- within(
            aggregate(arg, list(category = categories), sum), {
                    PcUnadj <- sqrt(propVar) } )
    
    propVar <- sum(byCategory$propVar)
    Pc      <- sqrt(propVar * RBiasFactor)
    PcUnadj <- sqrt(propVar)
    
    partialIndicator <- list(
            type             = 'Conditional partial R-indicator, sample based',
            variable         = variable,
            Pc               = Pc,
            PcUnadj          = PcUnadj,
            byCategory       = byCategory,
            propMeanByOthers = propMeanByOthers,
            zMeanByOthers    = zMeanByOthers)
 
    partialIndicator <- getVariancePartialRConditional(
            partialIndicator, indicator, sampleData, sampleDesign)
 
    partialIndicator$byCategory <-
            partialIndicator$byCategory[c('category', 'PcUnadj', 'PcUnadjSE')]
    
    return (partialIndicator)
}
