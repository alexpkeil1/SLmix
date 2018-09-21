ml.quantile_f <-
function (data, v_n, breaks){
    for (i in 1:length(v_n)) {
        dat_num = as.numeric(unlist(data[, v_n[i]]))
        data[[paste(v_n[i], "q", sep = "_")]] = cut(dat_num, 
            breaks = breaks[,i], labels = FALSE, 
            include.lowest = TRUE) - 1
    }
    return(data)
}
