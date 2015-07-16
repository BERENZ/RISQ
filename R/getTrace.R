getTrace <-
function(m)
{ #++
    # Returns the trace of the matrix m.
    
    return (sum(m[col(m) == row(m)]))
}
