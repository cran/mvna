"print.mvna" <-
function(x,...) {
  if (!inherits(x,"mvna"))
    stop("Argument 'x' must be of class 'mvna'")
  cat("\n")
  for (i in 1:(length(x)-7)) {
    a <- strsplit(names(x)[i]," ")
    q <- round(quantile(1:length(x[[i]]$na)))
    cat("Estimated cumulative hazard for transition",a[[1]][1],"to",a[[1]][2],"\n\n")
    cat("Time","\n")
    print(x[[i]]$time[q],...) ; cat("\n")
    cat("Nelson-Aalen estimates","\n")
    print(round(x[[i]]$na[q],3),...) ; cat("\n")
    cat("Variance estimates","\n")
    print(round(x[[i]]$var1[q],3),...) ; cat("\n")
    cat("Alternative variance estimates","\n")
    print(round(x[[i]]$var2[q],3),...) ; cat("\n\n\n")
  }
  if (!(is.null(x$cens.name))) {
    cat(sum(x$ncens),"observations were censored","\n")
  }
}
  
