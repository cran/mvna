mvna <- function(data,state.numbers,tra,cens.name) {
  if (missing(data))
    stop("Argument 'data' is missing with no default")
  if (missing(tra))
    stop("Argument 'tra' is missing with no default")
  if (missing(state.numbers))
    stop("Argument 'state.numbers' is missing with no default")
  if (missing(cens.name))
    stop("Argument 'cens.name' is missing with no default")
  if (!is.data.frame(data))
    stop("Argument 'data' must be a data.frame")
  if (!(xor(sum(c("id", "from", "to", "time") %in% names(data)) != 4,
            sum(c("id", "from", "to", "entry", "exit") %in% names(data)) != 5)))
    stop("'data' must contain the right variables")
  if (nrow(tra) != ncol(tra))
    stop("Argument 'tra' must be a quadratic  matrix.")
  if (sum(diag(tra)) > 0)
    stop("transitions into the same state are not allowed")
  if (nrow(tra) != length(state.numbers)) {
    stop("The row number of 'tra' must be equal to the number of states.")
  }
  if (!is.logical(tra)) {
    stop("'tra' must be a matrix of logical values, which describes the possible transitions.")
  }
  if (length(state.numbers) != length(unique(state.numbers))) {
    stop("The state numbers must be unique.")
  }
  if (!(is.null(cens.name))) {
    if (cens.name %in% state.numbers) {
      stop("The name of the censoring variable just is a name of the model states.")
    }
  }
  ### transitions
  colnames(tra) <- rownames(tra) <- state.numbers
  t.from <- lapply(1:dim(tra)[2], function(i) {
    rep(rownames(tra)[i], sum(tra[i, ]))
  })
  t.from <- unlist(t.from)
  t.to <- lapply(1:dim(tra)[2], function(i) {
    colnames(tra)[tra[i, ]==TRUE]
  })
  t.to <- unlist(t.to)
  trans <- data.frame(from=t.from, to=t.to)
  namen <- paste(trans[, 1], trans[, 2])
  # test on transitions
  test <- unique(paste(data$from, data$to))
  if (!(is.null(cens.name))) {
    ref <- c(paste(trans$from, trans$to), paste(unique(trans$from), cens.name))
  }
  else { ref <- paste(trans$from, trans$to) }
  if (!(all(ref %in% test)==TRUE))
    stop("There is undefined transitions in the data set")
  if (sum(as.character(data$from)==as.character(data$to)) > 0)
    stop("Transitions into the same state are not allowed")
  ### data.frame transformation
  data$from <- as.factor(data$from)
  data$to <- as.factor(data$to)
  factors <- sort(unique(c(levels(data$from), levels(data$to))))
  if (!(is.null(cens.name))) {
    data$from <- factor(data$from, levels=c(cens.name, factors[factors != cens.name]), ordered=TRUE)
    levels(data$from) <- 0:(length(factors) - 1)
    data$to <- factor(data$to, levels=c(cens.name, factors[factors != cens.name]), ordered=TRUE)
    levels(data$to) <- 0:(length(factors) - 1)
  }
  else {
    data$from <- factor(data$from, levels=factors, ordered=TRUE)
    levels(data$from) <- 1:length(factors)
    data$to <- factor(data$to, levels=factors, ordered=TRUE)
    levels(data$to) <- 1:length(factors)
  }
  # if not, put like counting process data
  if ("time" %in% names(data)) {
    data <- data[order(data$id, data$time), ] 
    entree <- double(length(data$time))
    entree[1] <- 0
    for (i in 2:nrow(data)) {
      if (data$id[i] != data$id[i-1]) {
        entree[i] <- 0
      }
      else {
        entree[i] <- data$time[i-1]
      }
    }
    data <- data.frame(id=data$id, from=data$from, to=data$to, entry=entree, exit=data$time)
    if (sum(data$entry < data$exit) != nrow(data))
      stop("Exit time from a state must be > entry time") 
  }
  else {
    if (sum(data$entry < data$exit) != nrow(data))
      stop("Exit time from a state must be > entry time")
  }
  ### Computation of the risk set and dN
  ttime <- c(data$entry, data$exit)
  times <- sort(unique(ttime))
  data$from <- as.integer(as.character(data$from))
  data$to <- as.integer(as.character(data$to))
  temp <- .C("risk_set",
             as.integer(nrow(data)),
             as.integer(length(times)),
             as.integer(c(length(unique(trans[, 1]))), length(times)),
             as.integer(c(dim(tra), length(times))),
             as.double(times),
             as.integer(data$from),
             as.integer(data$to),
             as.double(data$entry),
             as.double(data$exit),
             nrisk=integer(dim(tra)[1] * length(times)),
             ncens=integer(dim(tra)[1] * length(times)),
             nev=integer(dim(tra)[1] * dim(tra)[2] * length(times)),
             PACKAGE="mvna")
  nrisk <- matrix(temp$nrisk, ncol=dim(tra)[1], nrow=length(times))
  ncens <- matrix(temp$ncens, ncol=dim(tra)[1], nrow=length(times))
  nev <- array(temp$nev, dim=c(dim(tra), length(times)))
  if (sum(nrisk[1, ]==0)) nrisk[1, ] <- nrisk[2, ]
  ### computation of the NA estimates
  colnames(nrisk) <- state.numbers
  dimnames(nev) <- list(state.numbers, state.numbers, times)
  est <- lapply(1:nrow(trans), function(i) {
    t.nrisk <- nrisk[, as.character(trans[i, 1])][nrisk[, as.character(trans[i, 1])] != 0]
    t.nev <- nev[as.character(trans[i, 1]), as.character(trans[i, 2]), ][nrisk[, as.character(trans[i, 1])] != 0]
    na <- cumsum(t.nev/t.nrisk)
    var1 <- cumsum(t.nev/t.nrisk^2)
    var2 <- cumsum(((t.nrisk - t.nev)/t.nrisk^3) * t.nev)
    data.frame(na=na, var1=var1, var2=var2, time=times[nrisk[, as.character(trans[i, 1])] != 0])
  })
  nrisk <- nrisk[, !(colnames(nrisk) %in% setdiff(unique(trans$to), unique(trans$from)))]
  names(est) <- namen
  eest <- list(time=times, nrisk=nrisk, nev=nev, ncens=ncens,
               state.numbers=state.numbers, cens.name=cens.name,
               trans=trans)
  res <- c(est, eest)
  class(res) <- "mvna"
  res
}