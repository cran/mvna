"predict.mvna" <-
function(object,times,tr.choice,...) {
  if(!inherits(object,"mvna")) {
    stop("Argument 'object' must be of class \"mvna\".")
  }
  if (sum(times < 0) >=1)
    stop("Negative 'times' may be problematic")
  if (missing(tr.choice)) {
    tr.choice <- names(object[1:(length(object)-3)])
  }
  object <- object[tr.choice]
  if (sum(tr.choice %in% names(object)) != length(tr.choice)) {
    stop("Transition names given in 'tr.choice' must match with 'object'")
  }
  ltimes <- length(times)
  res <- vector("list",(length(object)))
  for (i in 1:(length(object))) {
    nt <- nrow(object[[i]])
    temp <- .C("pr",
            as.integer(nt),
            as.double(object[[i]]$na),
            as.double(object[[i]]$var1),
            as.double(object[[i]]$var2),
            as.double(object[[i]]$time),
            as.double(times),
            as.integer(ltimes),
            double(ltimes),
            double(ltimes),
            double(ltimes),
            PACKAGE="mvna")[c(8,9,10)]
    temp <- do.call("cbind",temp)
    temp <- data.frame(cbind(temp,times))
    names(temp) <- c("na","var1","var2","time")
    res[[i]] <- temp
  }
  names(res) <- names(object[1:(length(object))])
  res
}

