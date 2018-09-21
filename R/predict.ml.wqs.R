predict.ml.wqs <-
function(ml.wqs.res, newdata){
  #newx must be ordered -> first columns should be x variables, second group should be Z variables
  w = ml.wqs.res$weights
  q.train.quantiles = ml.wqs.res$training.quantiles
  dimw = length(w)
  dimx = dim(newdata)[2]
  q = ml.quantile.fn(newdata[,1:dimw], q.train.quantiles)
  newWQS <- as.numeric(q %*% w)
  newZ <- data.frame(WQS=newWQS)
  if(dimw<dimx){
    newZ <- cbind(newZ, newdata[,(dimw+1):dimx])
    names(newZ) <- cbind("WQS", names(newdata)[(dimw+1):dimx])

  }
 predict(ml.wqs.res$fit, newZ, envir = ml.wqs.res$env)
}
