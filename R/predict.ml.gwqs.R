predict.ml.gwqs <-
function(fit, mix_name, newdata){
  #newx must be ordered -> first columns should be x variables, second group should be Z variables
  qname = paste(mix_name, "q", sep = "_")
  w = fit$final_weights$mean_weight
  q.train.quantiles = fit$training.quantiles
  dimw = length(qname)
  dimx = dim(newdata)[2]
  q = ml.quantile_f(newdata, mix_name, q.train.quantiles)[,qname, drop=FALSE]
  newWQS <- as.numeric(as.matrix(q) %*% w)
  newZ <- data.frame(wqs=newWQS)
  if(dimw<dimx){
    newZ <- cbind(newZ, newdata[,(dimw+1):dimx])
    names(newZ) <- cbind("wqs", names(newdata)[(dimw+1):dimx])
  }
 predict.glm(fit$fit, newZ, envir = fit$env, type='response')
}
