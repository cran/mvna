n.events.mvna <- function(object,data,...) {
  if (!(inherits(object,"mvna")))
    stop("'object must be of class 'mvna'")
  if (missing(data))
    stop("Argument 'data' is missing with no default")
  if (!is.data.frame(data))
    stop("Argument 'data' must be a data.frame")
  if (!(xor(sum(c("id","from","to","time") %in% names(data)) != 4,
      sum(c("id","from","to","entry","exit") %in% names(data)) != 5)))
    stop("'data must contain the right variables")
  l <- length(object$state.numbers)
  name <- object$state.numbers
  n.trans <- matrix(ncol=l,nrow=l,0)
  dimnames(n.trans) <- list(name,name)
  if (!(is.na(object$cens.name))) {
    temp <- subset(data,data$to!=object$cens.name)
  }
  else temp <- data
  for (i in 1:nrow(temp)) {
    n.trans[as.character(temp$from[i]),
            as.character(temp$to[i])] <- n.trans[as.character(temp$from[i]),
                                                 as.character(temp$to[i])]+1
  }
  tot <- nrow(data)
  percent <- matrix(ncol=l,nrow=l,0)
  dimnames(percent) <- list(name,name)
  percent[,] <- round(n.trans[,]*100/tot)
  censoring <- c(object$n.cens,100-sum(percent))
  list(n.trans=n.trans,percent=percent,censoring=censoring)
}
  
