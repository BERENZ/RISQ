getSampleVarTotalPPS <-
function(sampleDesign,
                 y)
{ #++
    # If the sample design is neither SI nor STSI, use the formulae of the SE
    # of a PPS design.

    # return (getSampleCovTotalPPS(sampleDesign, y, y))
 
    n <- length(sampleDesign$weights)
    y <- y * sampleDesign$weights
    sampleVar <- sum((n * y - sum(y))^2) / n / (n - 1)
    
    return (sampleVar)
}
