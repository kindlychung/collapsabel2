head2 = function(x, m=6, n=NULL) {
    if(is.null(n)) {
        n = m
    }
    x[1:m, 1:n]
}

tail2 = function(x, m = 6, n = NULL) {
    if(is.null(n)) {
        n = m
    }
    e1 = nrow(x)
    e2 = ncol(x)
    i1 = (e1 - m + 1)
    i2 = (e2 - n + 1)
    x[i1:e1, i2:e2]
}
