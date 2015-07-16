getSampleDesign <-
function(sampleWeights,
                 sampleStrata)
{ #++
    # Guesses which type of sample desing is used, using the following rules.
    # (1) A single stratum and constant weights implies SI sampling.
    # (2) More than one stratum and constant weights per stratum implies STSI
    #     sampling.
    minmax <- sapply(split(sampleWeights, sampleStrata), range)
    constantWeights <- all(minmax[1,] == minmax[2,])
 
    nStrata <- length(levels(sampleStrata[, drop = TRUE]))
 
    if (constantWeights) {
        type <- ifelse(nStrata == 1, 'SI', 'STSI')
        getVarTotal <- getSampleVarTotalSTSI
 
    } else {
        type <- ''
        getVarTotal <- getSampleVarTotalPPS
    }
 
    sampleDesign <- list(
            type        = type,
            weights     = sampleWeights,
            strata      = sampleStrata,
            getVarTotal = getVarTotal)
 
    return (sampleDesign)
}
