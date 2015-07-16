replaceRHSByVariable <-
function(formula,
                 variable)
{ #++
    replacement <- as.formula(paste('. ~ ', variable, sep = ''))
    formula <- update.formula(formula, replacement)

    return (formula)
}
