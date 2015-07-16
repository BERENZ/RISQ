getPartialRs <-
function(indicator,
                 sampleData,
                 sampleDesign,
                 otherVariables = character())
{ #++
    # Estimates both unconditional and conditional partial R-indicators.

    modelVariables <- getVariables(indicator$model$formula, FALSE)
    variables <- unique(c(modelVariables, otherVariables))

    byVariables  <- NULL
    byCategories <- list()

    for (variable in variables) {
        pConditional <-
                getPartialRConditional(
                        indicator, variable, sampleData, sampleDesign)

       pUnconditional <-
                getPartialRUnconditional(
                        indicator, variable, sampleData, sampleDesign)

       byVariable <- data.frame(
                variable   = variable,
                Pu         = pUnconditional$Pu,
                PuUnadj    = pUnconditional$PuUnadj,
                PuSE       = pUnconditional$PuSE,
                Pc         = pConditional$Pc,
                PcUnadj    = pConditional$PcUnadj,
                PcSEApprox = pUnconditional$PuSE)

        byVariables <- rbind(byVariables, byVariable)

        byCategory <- merge(
                pUnconditional$byCategory,
                pConditional$byCategory)

        byCategories <- c(byCategories, list(byCategory))
    }

    names(byCategories) <- byVariables$variable

    partialRs <- list(
            byVariables  = byVariables,
            byCategories = byCategories)

    return (partialRs)
}
