mvna <- function(data,state.numbers,tra,cens.name,
                 tr.choice="all") {
  ### securities
  if (missing(data))
    stop("Argument 'data' is missing with no default")
  if (missing(tra))
    stop("Argument 'tra' is missing with no default")
  if (missing(state.numbers))
    stop("Argument 'state.numbers' is missing with no default")
  if (missing(cens.name))
    stop("Argument 'cens.name' is missing with no default")
  cens <- cens.name
  if (!is.data.frame(data))
    stop("Argument 'data' must be a data.frame")
  if (!(xor(sum(c("id","from","to","time") %in% names(data)) != 4,
      sum(c("id","from","to","entry","exit") %in% names(data)) != 5)))
    stop("'data must contain the right variables")
  if (!is.character(tr.choice))
    stop("Argument 'tr.choice' must be character vector")
  if (nrow(tra) != ncol(tra)) {
    stop("Argument 'tra' must be a quadratic  matrix.")
  }
  if (nrow(tra) != length(state.numbers)) {
    stop("The row number of 'tra' must be equal to the number of states.")
  }
  if (!is.logical(tra)) {
    stop("'tra' must be a matrix of logical values, which describes the possible transitions.")
  }
  if (length(state.numbers) != length(unique(state.numbers))) {
    stop("The state numbers must be unique.")
  }
  data$to <- as.factor(data$to)
  if (!is.na(cens.name)) {
    if (cens.name %in% state.numbers) {
      stop("The name of the censoring variable just is a name of the model states.")
    }
    if (!(cens.name %in% levels(data$to))) {
      stop("'cens.name' does not match with the data")
    }
  }
  else { cens.name <- "azerty"}
  lfrom <- levels(data$from)
  lto <- levels(data$to)
  cc <- unique(append(lfrom,lto))
  cc <- cc[cc!=cens.name]
  if (!(all(cc %in% state.numbers)))
    stop("'state.numbers' does not match with the data")
  ### computing possible transitions
  states <- 1:length(state.numbers)
  dimnames(tra) <- list(state.numbers, state.numbers)  
  transitions.from <- row(tra)[tra[, ] == TRUE][row(tra)[tra[, 
        ] == TRUE] != col(tra)[tra[, ] == TRUE]]
  transitions.to <- col(tra)[tra[, ] == TRUE][row(tra)[tra[, 
        ] == TRUE] != col(tra)[tra[, ] == TRUE]]
  states = c(states, length(states) + 1)
  transitions.to <- c(transitions.to, rep(length(states), length(unique(transitions.from))))
  transitions.from <- c(transitions.from, unique(transitions.from))
  transitions <- matrix(c(transitions.from, transitions.to), 
        nrow = length(transitions.from), ncol = 2, byrow = FALSE)
  transitions <- transitions[order(transitions[, 1], transitions[, 
        2]), ]
  transitions <- transitions[transitions[,2]!=max(transitions[,2]),]
  if (is.null(dim(transitions)))
    transitions[] <- state.numbers[transitions[]]
  else transitions[,] <- state.numbers[transitions[,]]
  mode(transitions) <- "numeric"
  ### transformations for argument tr
  nom <- NULL
  if (tr.choice[1]=="all") {
    trans <- transitions
    if (is.null(nrow(trans))) trans <- rbind(trans)
  }
  else {
    for (k in 1:nrow(transitions)) {
      nom[k] <- paste(transitions[k,1],transitions[k,2])
    }
    if (sum(tr.choice %in% nom) != length(tr.choice)) {
      stop("Argument 'tr.choice' and the possible transitions must match")
    }
    trans <- do.call(rbind,strsplit(tr.choice," "))
    mode(trans) <- "numeric"
  }
  ee <- unique(data.frame(de=data$from,vers=data$to))
  ee <- subset(ee,ee$vers != cens.name)
  nom <- NULL
  prenom <- NULL
  for (i in 1:nrow(ee)) {
    nom[i] <- paste(ee$de[i],ee$vers[i])
  }
  for (i in 1:nrow(trans)) {
    prenom[i] <- paste(trans[i,1],trans[i,2])
  }
  if (sum(prenom %in% nom) != length(prenom)) {
    cat("\n")
    cat("'tra' gives possible transitions that may not be possible in the data.","\n")
    cat("Type 'enter' to continue anyway.","\n")
    cat("Otherwise, type 'Q' to quit execution","\n")
    browser()
  }
  ### data transformations
  if ("time" %in% names(data)) {
    ddd <- split(data,data$id)
    newdata <- lapply(1:length(ddd),function(i) {
      index <- order(ddd[[i]]$time)
      temp <- ddd[[i]][index,] 
      temp$entry[1] <- 0 ; temp$exit[1] <- temp$time[1]+10^-13
      if (nrow(temp) > 1) {  
        for (j in 2:nrow(temp)) {
          temp$exit[j] <- temp$time[j]+10^-13
          temp$entry[j] <- temp$time[j-1]
        }
      }
    return(temp)
    } )
    newdata <- do.call('rbind',newdata)
  }
  else {
    newdata <- data
    newdata$exit <- newdata$exit+10^-13
  }
  for (i in 1:nrow(newdata)) {
    if (newdata$to[i] != cens.name)
      newdata$ev[i] <- as.numeric(levels(newdata$to)[newdata$to[i]])+1
    else newdata$ev[i] <- 0
  }
  ### Computation of the Nelson--Aalen estimator
  est <- vector("list",nrow(trans))
  pfr <- unique(trans[,1])
  k <- 1
  for (i in 1:length(pfr)) {
    ptr <- subset(trans,trans[,1]==pfr[i])
    for (j in 1:nrow(ptr)) {
      res <- coxph(Surv(entry,exit,ev==ptr[j,2]+1)~1,
                      data=newdata,subset=newdata[,"from"]==pfr[i])
      temp <- survfit(res)
      na <- cumsum(temp$n.event/temp$n.risk)
      var1 <- cumsum(temp$n.event/temp$n.risk^2)
      var2 <- cumsum(((temp$n.risk-temp$n.event)/temp$n.risk^3)*temp$n.event)
      if (length(temp$n.risk[temp$n.risk<=5])>0)
        warning("From time ", round(temp$time[temp$n.risk<=5][1]),", the risk set is lower than 5
for transition ",ptr[j,1], " to ", ptr[j,2],
".\nThe variance estimates may be biased.
See 'help(mvna)' for details.\n")
      est[[k]] <- data.frame(na,var1,var2,time=temp$time)
      k <- k+1
      }
  }
  est[[length(est)+1]] <- state.numbers
  est[[length(est)+1]] <- cens
  est[[length(est)+1]] <- sum(data$to==cens.name)
  name <- paste(trans[,1],trans[,2])
  names(est) <- c(name,"state.numbers","cens.name","n.cens")
  class(est) <- "mvna"
  est
  }


