funByLen = function(x, species, bins, cb, ...) {
  ## Make an empty list
  n = length(bins)
  l = list();
  ## callback value below lowest bin value
  l[[1]] = cb(x, species, len_lt = bins[1], ...)
  l[[1]]$LEN = rep(paste("<", bins[1]), nrow(l[[1]]))
  ## Calculate between bin values
  if (n > 1){
    for(i in 1:(n-1)){
      l[[i+1]] = cb(x, species, len_geq = bins[i], len_lt = bins[i+1], ...)
      l[[i+1]]$LEN = rep(paste('[', bins[i], ', ', bins[i+1], ')', sep = ""), nrow(l[[i+1]]))
    }
  }
  l[[n+1]] = cb(x, species, len_geq = bins[n], ...)
  l[[n+1]]$LEN = rep(paste(">=", bins[n]), nrow(l[[n+1]]))
  l[[n+2]] = cb(x, species, ...)
  l[[n+2]]$LEN = rep("all", nrow(l[[n+2]]))

  return(do.call(rbind, l))
}
