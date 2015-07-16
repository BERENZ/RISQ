getVariables <-
function(formula,
                 leftHandSide = FALSE)
{ #++
    # Returns the names of the variables used either in the left hand side of
    # the formula or in the right hand side of the formula.

    if (leftHandSide)
        formula <- update.formula(formula, . ~ 1)
    else
        formula <- update.formula(formula, 1 ~ .)
    
    variables <- all.vars(formula)
    
    if (length(variables) == 1 && variables == '.')
        variables <- NA
    
    return (variables)
}
