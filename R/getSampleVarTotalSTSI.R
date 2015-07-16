getSampleVarTotalSTSI <-
function(sampleDesign,
                 y)
{ #++
    # return (getSampleCovTotalSTSI(sampleDesign, y, y))

    getStratumVarTotal <-
            function(sample)
    { #++
        N <- sum(sample$weights)
        n <- nrow(sample)

        return (N^2 * (1 - n / N) * var(sample$y) / n)
    } #--
    
    sample <- data.frame(y = y, weights = sampleDesign$weights)
    strataVar <- sapply(split(sample, sampleDesign$strata), getStratumVarTotal)
    sampleVar <- sum(strataVar)
 
    return (sampleVar)
}
