getVariancePartialRConditional <-
function(partialIndicator,
                 indicator,
                 sampleData,
                 sampleDesign)
{ #++
    byCategory  <- partialIndicator$byCategory
    variable    <- partialIndicator$variable
    sigma       <- indicator$sigma
    weights     <- sampleDesign$weights
    nPopulation <- sum(weights)

    propDelta <- indicator$prop - partialIndicator$propMeanByOthers
    zDelta <- indicator$z - partialIndicator$zMeanByOthers

    variance <- numeric()
    for (index in seq(nrow(byCategory))) {
        label <- byCategory[index, 'category']
        delta <- ifelse(sampleData[[variable]] == label, 1, 0)
        zDeltaWeight <- zDelta * delta * weights
        propDelta2 <- propDelta * propDelta * delta

        A <- matrix(propDelta, nrow = 1) %*% zDeltaWeight
        B <- t(zDelta) %*% zDeltaWeight

        V1 <- 4 * A %*% sigma %*% t(A)
        V2 <- 2 * getTrace(B %*% sigma %*% B %*% sigma)
        V3 <- sampleDesign$getVarTotal(sampleDesign, propDelta2)

        variance[index] <-
                0.25 * (V1 + V2 + V3) /
                (nPopulation * sum(propDelta2 * weights))
    }

    partialIndicator$byCategory <- within(byCategory,
            PcUnadjSE <- sqrt(variance))
 
    return (partialIndicator)
}
