ml.wqs.est <-
function (y.train, x.train, z.train = NULL, y.valid = y.train, x.valid = x.train, z.valid = z.train, n.quantiles = 4, B = 100, b1.pos = TRUE) {
    check_train <- wqs:::check_xyz(x.train, y.train, z.train)
    if (check_train[1]) 
        stop("x.train must be a matrix")
    if (check_train[2]) 
        stop("check dimensions of training data")
    if (check_train[3]) 
        stop("check dimensions of training data")
    check_valid <- wqs:::check_xyz(x.valid, y.valid, z.valid)
    if (check_valid[1]) 
        stop("x.valid must be a matrix")
    if (check_valid[2]) 
        stop("check dimensions of validation data")
    if (check_valid[3]) 
        stop("check dimensions of validation data")
    if (B < 2) 
        stop("value of B must be at least 2")
    if (class(b1.pos) != "logical") 
        stop("b1.pos must be logical value of TRUE or FALSE")
    if (n.quantiles < 2 | n.quantiles > 10) 
        stop("n.quantiles must be at least 2 and no greater than 10")
    c <- dim(x.train)[2]
    q.train.quantiles <- ml.getcuts(x.train, n.quantiles)
    q.train <- ml.quantile.fn(x.train, q.train.quantiles)
    # this seems incorrect: if validation data is one datapoint, what would we do?
    q.valid <- wqs:::quantile.fn(x.valid, n.quantiles)
    bounds <- wqs:::specify.bounds(b1.pos, c)
    ineqLB <- bounds$ineqLB
    ineqUB <- bounds$ineqUB
    init <- wqs:::specify.init(z.train, y.train, b1.pos, c)
    result <- wqs:::wqs_b.est(y.train, q.train, z.train, B, pars = init, 
        fun = wqs:::objfn.cont, eqfun = wqs:::lincon, eqB = 1, ineqfun = wqs:::ineq, 
        ineqLB, ineqUB, LB = NULL, UB = NULL)
    wts.matrix <- result$wts.matrix
    weights <- wqs:::teststat.fn(wts.matrix, result$test_stat)
    final <- wqs:::wqs.fit(q.valid, z.valid, y.valid, weights)
    out <- list(q.train, q.valid, wts.matrix, weights, final$WQS, 
        final$fit, q.train.quantiles)
    names(out) <- c("q.train", "q.valid", "wts.matrix", "weights", 
        "WQS", "fit", "training.quantiles")
    return(out)
}
