"xyplot.mvna" <-
function(x,data=NULL,xlab="Time",ylab="Cumulative Hazard",
         tr.choice="all",conf.int=TRUE,ci.type=c(1,2),level=0.05,
         col=c(1,1,1),lty=c(1,3,3),...) {
  if (!inherits(x,"mvna")) {
    stop("Argument 'x' must be of class 'mvna'")
  }
  if (tr.choice[1]=="all") { tr.choice <- names(x[1:(length(x)-7)]) }
  if (!all(tr.choice %in% names(x))) {
    stop("Argument 'tr.choice' and the possible transitions must match")}
  if (!(length(ci.type)==2 & ci.type[1] %in% c(1,2) & ci.type[2] %in% c(1,2,3)))
    stop("Argument 'ci.type' is not in the good range. See documentation files")
  if (level < 0 | level > 1)
    stop("Argument 'level' must range between 0 and 1")
  sx <- x[tr.choice]
  for (i in 1:length(sx)) {
    sx[[i]]$cov <- as.factor(rep(names(sx)[i],nrow(sx[[i]])))
  }
  newx <- do.call(rbind,sx)
  if (conf.int==FALSE) {
    xyplot(newx$na ~ newx$time | newx$cov, type="s",
           col=col[1],lty=lty[1],xlab=xlab,ylab=ylab,...)
  }
  else {
    if (sum(ci.type==c(1,1))==2) {
      ciplus <- newx$na+qnorm(1-level/2)*sqrt(newx$var1)
      cimoins <- newx$na-qnorm(1-level/2)*sqrt(newx$var1)
    }
    if (sum(ci.type==c(1,2))==2) {
      ciplus <- newx$na*exp((qnorm(1-level/2)*sqrt(newx$var1))/newx$na)
      cimoins <- newx$na*exp((-qnorm(1-level/2)*sqrt(newx$var1))/newx$na)
    }
    if (sum(ci.type==c(1,3))==2) {
      ciplus <- -2*log(sin(pmax(0,asin(exp(-newx$na/2))
                               -(1/2)*qnorm(1-level/2)*newx$var1*(exp(newx$na)-1)^(-1/2))))
      cimoins <- -2*log(sin(pmin(pi/2,asin(exp(-newx$na/2))
                                +(1/2)*qnorm(1-level/2)*newx$var1*(exp(newx$na)-1)^(-1/2))))
    }
    if (sum(ci.type==c(2,1))==2) {
      ciplus <- newx$na+qnorm(1-level/2)*sqrt(newx$var2)
      cimoins <- newx$na-qnorm(1-level/2)*sqrt(newx$var2)
    }
    if (sum(ci.type==c(2,2))==2) {
      ciplus <- newx$na*exp((qnorm(1-level/2)*sqrt(newx$var2))/newx$na)
      cimoins <- newx$na*exp((-qnorm(1-level/2)*sqrt(newx$var2))/newx$na)
    }
    if (sum(ci.type==c(2,3))==2) {
      ciplus <- -2*log(sin(pmax(0,asin(exp(-newx$na/2))
                               -(1/2)*qnorm(1-level/2)*newx$var2*(exp(newx$na)-1)^(-1/2))))
      cimoins <- -2*log(sin(pmin(pi/2,asin(exp(-newx$na/2))
                                +(1/2)*qnorm(1-level/2)*newx$var2*(exp(newx$na)-1)^(-1/2))))
    }
    xyplot(newx$na + ciplus + cimoins ~ newx$time | newx$cov,
           type="s",col=col,lty=lty,xlab=xlab,ylab=ylab,...)
  }
}

