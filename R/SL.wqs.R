SL.wqs <-
function (Y, X, newX, family, obsWeights, id, B=20, n.quantiles=4, ...) {
    if (is.matrix(X)) {
        X = as.data.frame(X)
    }
    if (is.matrix(newX)) {
        newX = as.data.frame(newX)
    }
    if (family$family == "gaussian") {
      wqsfit = ml.wqs.est(y.train=Y, x.train=X, z.train=NULL, n.quantiles = n.quantiles, B=B, b1.pos=TRUE)
    }
    if (family$family == "binomial") {
      wqsfit = ml.wqs.est(y.train=Y, x.train=X, z.train=NULL, n.quantiles = n.quantiles, B=B, b1.pos=TRUE)
    }
    pred <- predict(wqsfit, newdata=newX)
    class(wqsfit) <- c("SL.wqs")
    out <- list(pred = pred, fit = wqsfit)
    return(out)
}
