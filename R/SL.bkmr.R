SL.bkmr <-
function(Y, X, newX, family, obsWeights, id, ...){
    if (is.matrix(X)) {
        X = as.data.frame(X)
    }
    if (is.matrix(newX)) {
        newX = as.data.frame(newX)
    }
    if (family$family == "gaussian") {
      fit = kmbayes(y=Y,Z=X, iter = 1000, family = "gaussian", verbose = FALSE)
    }
    if (family$family == "binomial") {
      fit = kmbayes(y=Y,Z=X, iter = 1000, family = "binomial", verbose = FALSE)
    }
    pMat <- SamplePred(fit, Znew=newX, Xnew = cbind(0), type='response')
    pred <- as.numeric(apply(pMat, 2, mean))
    class(fit) <- c("SL.bkmr")
    out <- list(pred = pred, fit = fit)
    return(out)
}
