getBiasRSampleBased <-
function(prop,
                 z,
                 sigma,
                 sampleDesign)
{ #++
    # Estimates the bias of the estimator for the variance of the
    # propensities.

    nPopulation <- sum(sampleDesign$weights)
    propVar <- sampleDesign$getVarTotal(sampleDesign, prop)
    z <- z * sqrt(sampleDesign$weights)

    lambda1 <- sum(apply(z, 1, function(zi) return(t(zi) %*% sigma %*% zi)))
    lambda2 <- propVar / nPopulation
    bias <- (lambda1 - lambda2) / nPopulation

    return (bias)
}
