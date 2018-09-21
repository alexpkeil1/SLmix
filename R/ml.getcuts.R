ml.getcuts <-
function(data, n.quantiles){
    cuts <- matrix(0, n.quantiles+1, dim(data)[2])
    I <- dim(data)[2]
    for (i in 1:I) {
        cuts[,i] <- quantile(data[, i], probs = c(0:n.quantiles/n.quantiles))
        # guarantee coverage
        cuts[1,i] <- -1e32
        cuts[n.quantiles+1,i] <- 1e32
    }
    return(cuts)
}
