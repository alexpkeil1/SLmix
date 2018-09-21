ml.quantilefind_f <-
function (data, v_n, q){
    ncols = dim(data)[2]
    br = matrix(0, q+1, length(v_n))
    for (i in 1:length(v_n)) {
        dat_num = as.numeric(unlist(data[, v_n[i]]))
        br[,i] = quantile(dat_num, probs = seq(0, 1, by = 1/q), na.rm = TRUE)
        br[1,i] = -1e32
        br[q+1,i] = 1e32
    }
    return(br)
}
