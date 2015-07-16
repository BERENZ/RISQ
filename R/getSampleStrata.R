getSampleStrata <-
function(sampleWeights,
                 nMaxStrata = 20)
{ #++
    # Guesses a definition of the sample strata, using the values of the sample
    # weights.

    weights <- sort(unique(sampleWeights))
    indices <- seq(along = sampleWeights)
    strata <- factor(seq(along = weights))
    
    if (length(weights) <= nMaxStrata) {
        values <- merge(
                data.frame(weight = sampleWeights, index = indices),
                data.frame(weight = weights, stratum  = strata),
                all.x = TRUE)
    
        sampleStrata <- values[order(values$index), 'stratum']
    } else
        sampleStrata <- factor(rep(1, length(sampleWeights)))
    
    return (sampleStrata)
}
