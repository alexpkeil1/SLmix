SL.gwqs <-
function (Y, X, newX, family, obsWeights, id, b=20, q=4, validation = 0.0, ...) {
    nm = names(X)
    if (is.matrix(X)) {
        X = as.data.frame(X)
    }
    if (is.matrix(newX)) {
        newX = as.data.frame(newX)
    }
    X$Y = Y
    if (family$family == "gaussian") {
      gwqsfit = ml.gwqs(Y ~ NULL, mix_name=nm, data=X, q = q, b=b, b1_pos=TRUE, family='gaussian')
    }
    if (family$family == "binomial") {
      gwqsfit = ml.gwqs(Y ~ NULL, mix_name=nm, data=X, q = q, b=b, b1_pos=TRUE, family='binomial')
    }
    pred <- predict.ml.gwqs(fit=gwqsfit, mix_name=nm, newdata=newX)
    class(gwqsfit) <- c("SL.gwqs")
    out <- list(pred = pred, fit = gwqsfit)
    return(out)
}
