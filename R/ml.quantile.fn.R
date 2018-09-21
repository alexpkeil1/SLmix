ml.quantile.fn <-
function (data, quantiles) {
    # use external quantiles
    q <- matrix(0, dim(data)[1], dim(data)[2])
    I <- dim(data)[2]
    for (i in 1:I) {
        q[,i] <- cut(data[, i], breaks = quantiles[, i], include.lowest = TRUE)
    }
    q <- q - 1
    return(q)
}
