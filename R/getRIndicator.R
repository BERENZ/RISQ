getRIndicator <-
function(formula,
             sampleData,
             sampleWeights  = rep(1, nrow(sampleData)),
             sampleStrata   = NULL,
             family         = c('binomial', 'gaussian'),
             withPartials   = TRUE,
             otherVariables = character())
{ #++
    # Determines the R-indicators and the partial R-indicators for a sample.
    #
    # ARGUMENTS
    #   formula        : the respons model which will be used to determine the
    #                    R-indicators; the left hand side of the formula states
    #                    the respons variabele, the right hand side states the
    #                    lineair model of auxiliary variabeles which will be
    #                    used to describe the respons;
    #
    #   sampleData     : a data frame containing the sample data;
    #
    #   sampleWeights  : (optional) a vector with the inclusion weights of the
    #                    sampling units;
    #
    #   sampleStrata   : (optional) a vector with the strata membership of the
    #                    sampling units;
    #
    #   family         : (optional) a string either 'binomial' for logistic
    #                    regression or 'gaussian' for lineair regression;
    #
    #   withPartials   : (optional) a boolean value, indicating if partial
    #                    R-indicators have to be determined (TRUE) or
    #                    not (FALSE);
    #
    #   otherVariables : (optional).
    #
    # VALUE
    #   getRIndicator returns a list of which the most important components
    #   are described in the manual.

    nSample = nrow(sampleData)
    stopifnot(length(sampleWeights) == nSample)
    stopifnot(is.numeric(sampleWeights))

    # If sampleStrata is not defined, use sampleWeights to guess the values of
    # sampleStrata.
    if (is.null(sampleStrata))
        sampleStrata <- getSampleStrata(sampleWeights)

    stopifnot(is.factor(sampleStrata))
    stopifnot(length(sampleStrata) == nSample)

    sampleDesign <- getSampleDesign(sampleWeights, sampleStrata)

    family <- match.arg(family)
    model <- switch(family,
            'binomial' = list(
                    formula = formula,
                    grad    = function(mu)  exp(mu) / (1 + exp(mu))^2,
                    family  = binomial(link = 'logit')),
            'gaussian' = list(
                    formula = formula,
                    grad    = function(mu) 1,
                    family  = gaussian(link = 'identity')))
 
    indicator <- getRSampleBased(model, sampleData, sampleDesign)

    if (withPartials)
        indicator$partials <- getPartialRs(
                indicator, sampleData, sampleDesign, otherVariables)

    return (indicator)
}
