getVariancePartialRUnconditional <-
function(partialIndicator,
                 indicator,
                 sampleData,
                 sampleDesign)
{ #++
    # Calculate the variance of the partial-R indicators.

    nPopulation <- sum(sampleDesign$weights)
    variable    <- partialIndicator$variable
    byCategory  <- partialIndicator$byCategory
    prop        <- indicator$prop
 
    V1 <- numeric()
    V2 <- numeric()

    nSample <- nrow(sampleData)
 
    for (index in seq(nrow(byCategory))) {
        label  <- byCategory[index, 'category']
	    delta  <- ifelse(sampleData[[variable]] == label, 1, 0)
        deltaC <- 1 - delta

        V1[index] <- sampleDesign$getVarTotal(sampleDesign, delta * prop) 
        V2[index] <- sampleDesign$getVarTotal(sampleDesign, deltaC * prop)
    }
   
    partialIndicator$byCategory <- within(byCategory, {
            PuUnadjSE <- sqrt(n / nPopulation * (
                    V1 * (1 / n - 1 / nPopulation)^2 +
                    V2 * (1 / nPopulation)^2)) })
 
    return (partialIndicator)
}
