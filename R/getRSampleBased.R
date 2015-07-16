getRSampleBased <-
function(model,
                 sampleData,
                 sampleDesign)
{ #++
    # Determines the sample-based R-indicator.

    #  See 9.9 from Regression Modelling Strategies (Harrell, 2001) for a
    #  motivation for the factor
    #
    #    mean(sampleWeights) = sum(sampleWeights) / length(sampleWeights).

    sampleData <- within(sampleData, {
            sampleWeights <- sampleDesign$weights
            sampleWeights <- sampleWeights / mean(sampleWeights) })

    modelfit <- glm(model$formula, model$family, sampleData, sampleWeights)
    prop     <- predict(modelfit, type = 'response')
    propMean <- weighted.mean(prop, sampleDesign$weights)
    propVar  <- weightedVar(prop, sampleDesign$weights)

    # Because estimaters of bias and variance both use the following vectors
    # and matrix, they are calculated only once and passed to the functions.
    sigma <- vcov(modelfit)
    x <- model.matrix(model$formula, sampleData)[, colnames(sigma)]
    z <- model$grad(predict(modelfit, type = 'link')) * x

    withBiasAndVar <- !is.null(sigma) && all(!is.na(sigma))
    if (withBiasAndVar) {
        RBias <- getBiasRSampleBased(prop, z, sigma, sampleDesign)
        RVar  <- getVarianceRSampleBased(prop, z, sigma, sampleDesign)

        # To simplify formulas the bias correction of the variance will be
        # written as a factor, 1 - bias / (variance of propensities).
        if (RBias > propVar)
            RBiasFactor <- 0
        else
            RBiasFactor <- 1 - RBias / propVar
    } else {
        RBias <- NA
        RBiasFactor <- NA
        RVar <- NA
    }

    CV <- sqrt(propVar) / propMean
    CVVar <- 0.25 * RVar / propMean^2 + CV^4 / nrow(sampleData)

    indicator <- list(
            type          = 'R-indicator, sample based',
            sampleDesign  = sampleDesign,
            prop          = prop,
            propMean      = propMean,
            model         = model,
            modelfit      = modelfit,
            sigma         = sigma,
            z             = z,
            R             = 1 - 2 * sqrt(propVar * RBiasFactor),
            RUnadj        = 1 - 2 * sqrt(propVar),
            RSE           = sqrt(RVar),
            RBiasFactor   = RBiasFactor,
            CV            = CV,
            CVSE          = sqrt(CVVar))

    return (indicator)
}
