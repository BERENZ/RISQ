weightedVar <-
function(x,
                 weights = rep(1, length(x)),
                 method = c('unbiased', 'ML'))
{ #++
    # Returns the weighted variance of the vector x.

    xMean <- weighted.mean(x, weights)
    xVar <- sum(weights * (x - xMean)^2)
    xVar <- switch(match.arg(method),
                'unbiased' = xVar / (sum(weights) - 1),
                'ML'       = xVar / sum(weights))
    
    return (xVar)
}
