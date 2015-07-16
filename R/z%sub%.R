`%sub%` <-
function(x,
                 y)
{ #++
    # Returns all elements of the set operation x - y.
    #     > c(1, 2, 3, 4, 5) %sub% c(2, 4)
    #     [1] 1 3 5

    return (x[! x %in% y])
}
