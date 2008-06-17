"predict.mvna" <-
function(object,times,tr.choice,...) {
  if(!inherits(object,"mvna")) {
    stop("Argument 'object' must be of class \"mvna\".")
  }
  if (sum(times < 0) >=1)
    stop("Negative 'times' may be problematic")
  if (missing(tr.choice)) {
    tr.choice <- names(object[1:(length(object)-7)])
  }
  object <- object[tr.choice]
  if (sum(tr.choice %in% names(object)) != length(tr.choice)) {
    stop("Transition names given in 'tr.choice' must match with 'object'")
  }
  res <- lapply(1:length(object), function(i) {
    ind <- findInterval(times, object[[i]]$time)
    ind[ind==0] <- NA
    temp <- object[[i]][ind, ]
    temp[is.na(temp$time), ] <- 0
    temp$time <- times
    temp
  })
  res
}
    
