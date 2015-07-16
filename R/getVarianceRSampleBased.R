getVarianceRSampleBased <-
function(prop,
                 z,
                 sigma,
                 sampleDesign)
{ #++
    # Estimates the variance of the estimator for the R-indicator.
 
    weights <- sampleDesign$weights
    nSample <- length(weights)
    nPopulation <- sum(weights)

    propMean <- weighted.mean(prop, weights)
    propVar <- weightedVar(prop, weights, method = 'ML')
    propZ <- cbind(prop, z)

    A <- cov.wt(propZ, wt = weights, method = 'ML')$cov[-1, 1]
    B <- cov.wt(z, wt = weights, method = 'ML')$cov
    C <- sampleDesign$getVarTotal(sampleDesign, (prop - propMean)^2)

    variance <- numeric()
    variance[1] <- 4 * t(A) %*% sigma %*% A
    variance[2] <- 2 * getTrace(B %*% sigma %*% B %*% sigma)
    variance[3] <- C / nPopulation^2
    variance <- sum(variance) / propVar

    return (variance)
}
