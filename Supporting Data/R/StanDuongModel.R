#' @import rstan

detachPackage("snowfall")
detachPackage("doSNOW")
detachPackage("snow")
library(rstan)
library(parallel)
options(mc.cores = parallel::detectCores())
#options(mc.cores = 1)
#control=list(adapt_delta=0.9)

#StanDuongResults <- stan(
#  model_code = DuongModel,
#  data = StanProduction,
#  iter = iters
#)

CompiledDuongModel <- stan_model(file=paste(workdir, "R/", "DuongModel.stan", sep=""),auto_write=TRUE)
#CompiledDuongModel <- stan_model(file=paste(workdir, "R/", "DuongModel.stan", sep=""),auto_write=FALSE)

StanDuongModel <- function(x){
  UWI <- names(x[1])
  x3 <- subset(x[[1]],q>0)
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(diff(c(0,x3$Q))/x3$q),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=0.0,
    Q_prior=0.0
  )
  
  #plot(StanProduction$t, (StanProduction$q/StanProduction$Q), log="xy",xlim=c(1,10000),ylim=c(0.0001,1))
  #flush.console()
  
  DuongResults <- sampling(
    CompiledDuongModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","m","s3")
  )
  
  posterior <- as.data.frame(extract(DuongResults))
  posterior$qinf <- NA
  posterior$qi <- NA
  posterior$EUR <- NA
  
  for(k in 1:length(posterior$a)) {
    #for(k in 1:6) {
    a <- posterior$a[k]
    m <- posterior$m[k]
    tam <- (StanProduction$t^(-m))*exp((a/(1.0-m))*((StanProduction$t^(1.0-m))-1.0))
    results <- coef(lm(StanProduction$q ~ tam ))
    qinf <- results[1]
    qi <- results[2]
    posterior$qinf[k] <- qinf
    posterior$qi[k] <- qi
    
    posterior$EUR[k] <- StanProduction$cumtodate;
    
    t_star <- 30.4375*(1:600)
    if (m==1) {
      tam_star <- t_star^(a-1.0)
    } else {
      tam_star <- (t_star^(-m))*exp((a/(1.0-m))*((t_star^(1.0-m))-1.0))
    }
    q_star <- (qi*tam_star + qinf)[(qi*tam_star + qinf)>StanProduction$qf]
    posterior$EUR[k] <- max(StanProduction$cumtodate, (tail(q_star,1)/a)*t_star[length(q_star)]^m)
  }
  
  #print(head(posterior))
  #flush.console()
  closeAllConnections()
  return(posterior)
}

StanDuongModel03 <- function(x){
  UWI <- names(x[1])
  x3 <- tail(head(subset(x[[1]],q>0),6),36)
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(diff(c(0,x3$Q))/x3$q),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=0.0,
    Q_prior=0.0
  )
  
  #plot(StanProduction$t, (StanProduction$q/StanProduction$Q), log="xy",xlim=c(1,10000),ylim=c(0.0001,1))
  #flush.console()
  
  DuongResults <- sampling(
    CompiledDuongModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","m","s3")
  )
  
  posterior <- as.data.frame(extract(DuongResults))
  posterior$qinf <- NA
  posterior$qi <- NA
  posterior$EUR <- NA
  
  for(k in 1:length(posterior$a)) {
    #for(k in 1:6) {
    a <- posterior$a[k]
    m <- posterior$m[k]
    tam <- (StanProduction$t^(-m))*exp((a/(1.0-m))*((StanProduction$t^(1.0-m))-1.0))
    results <- coef(lm(StanProduction$q ~ tam ))
    qinf <- results[1]
    qi <- results[2]
    posterior$qinf[k] <- qinf
    posterior$qi[k] <- qi
    
    posterior$EUR[k] <- StanProduction$cumtodate;
    
    t_star <- 30.4375*(1:600)
    if (m==1) {
      tam_star <- t_star^(a-1.0)
    } else {
      tam_star <- (t_star^(-m))*exp((a/(1.0-m))*((t_star^(1.0-m))-1.0))
    }
    q_star <- (qi*tam_star + qinf)[(qi*tam_star + qinf)>StanProduction$qf]
    posterior$EUR[k] <- max(StanProduction$cumtodate, (tail(q_star,1)/a)*t_star[length(q_star)]^m)
  }
  
  #print(head(posterior))
  #flush.console()
  closeAllConnections()
  return(posterior)
}

StanDuongModel06 <- function(x){
  UWI <- names(x[1])
  x2 <- subset(x[[1]],q>0)
  x3 <- tail(head(x2,12),36)
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(diff(c(0,x3$Q))/x3$q),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=0.0,
    Q_prior=0.0
  )
  
  #plot(StanProduction$t, (StanProduction$q/StanProduction$Q), log="xy",xlim=c(1,10000),ylim=c(0.0001,1))
  #flush.console()
  
  DuongResults <- sampling(
    CompiledDuongModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","m","s3")
  )
  
  posterior <- as.data.frame(extract(DuongResults))
  posterior$qinf <- NA
  posterior$qi <- NA
  posterior$EUR <- NA
  
  for(k in 1:length(posterior$a)) {
    #for(k in 1:6) {
    a <- posterior$a[k]
    m <- posterior$m[k]
    tam <- (StanProduction$t^(-m))*exp((a/(1.0-m))*((StanProduction$t^(1.0-m))-1.0))
    results <- coef(lm(StanProduction$q ~ tam ))
    qinf <- results[1]
    qi <- results[2]
    posterior$qinf[k] <- qinf
    posterior$qi[k] <- qi
    
    posterior$EUR[k] <- StanProduction$cumtodate;
    
    t_star <- 30.4375*(1:600)
    if (m==1) {
      tam_star <- t_star^(a-1.0)
    } else {
      tam_star <- (t_star^(-m))*exp((a/(1.0-m))*((t_star^(1.0-m))-1.0))
    }
    q_star <- (qi*tam_star + qinf)[(qi*tam_star + qinf)>StanProduction$qf]
    posterior$EUR[k] <- max(StanProduction$cumtodate, (tail(q_star,1)/a)*t_star[length(q_star)]^m)
  }
  
  #print(head(posterior))
  #flush.console()
  closeAllConnections()
  return(posterior)
}

StanDuongModel09 <- function(x){
  UWI <- names(x[1])
  x2 <- subset(x[[1]],q>0)
  x3 <- tail(head(x2,18),36)
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(diff(c(0,x3$Q))/x3$q),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=0.0,
    Q_prior=0.0
  )
  
  #plot(StanProduction$t, (StanProduction$q/StanProduction$Q), log="xy",xlim=c(1,10000),ylim=c(0.0001,1))
  #flush.console()
  
  DuongResults <- sampling(
    CompiledDuongModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","m","s3")
  )
  
  posterior <- as.data.frame(extract(DuongResults))
  posterior$qinf <- NA
  posterior$qi <- NA
  posterior$EUR <- NA
  
  for(k in 1:length(posterior$a)) {
    #for(k in 1:6) {
    a <- posterior$a[k]
    m <- posterior$m[k]
    tam <- (StanProduction$t^(-m))*exp((a/(1.0-m))*((StanProduction$t^(1.0-m))-1.0))
    results <- coef(lm(StanProduction$q ~ tam ))
    qinf <- results[1]
    qi <- results[2]
    posterior$qinf[k] <- qinf
    posterior$qi[k] <- qi
    
    posterior$EUR[k] <- StanProduction$cumtodate;
    
    t_star <- 30.4375*(1:600)
    if (m==1) {
      tam_star <- t_star^(a-1.0)
    } else {
      tam_star <- (t_star^(-m))*exp((a/(1.0-m))*((t_star^(1.0-m))-1.0))
    }
    q_star <- (qi*tam_star + qinf)[(qi*tam_star + qinf)>StanProduction$qf]
    posterior$EUR[k] <- max(StanProduction$cumtodate, (tail(q_star,1)/a)*t_star[length(q_star)]^m)
  }
  
  #print(head(posterior))
  #flush.console()
  closeAllConnections()
  return(posterior)
}

StanDuongModel12 <- function(x){
  UWI <- names(x[1])
  x2 <- subset(x[[1]],q>0)
  x3 <- tail(head(x2,24),36)
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(diff(c(0,x3$Q))/x3$q),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=0.0,
    Q_prior=0.0
  )
  
  #plot(StanProduction$t, (StanProduction$q/StanProduction$Q), log="xy",xlim=c(1,10000),ylim=c(0.0001,1))
  #flush.console()
  
  DuongResults <- sampling(
    CompiledDuongModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","m","s3")
  )
  
  posterior <- as.data.frame(extract(DuongResults))
  posterior$qinf <- NA
  posterior$qi <- NA
  posterior$EUR <- NA
  
  for(k in 1:length(posterior$a)) {
    #for(k in 1:6) {
    a <- posterior$a[k]
    m <- posterior$m[k]
    tam <- (StanProduction$t^(-m))*exp((a/(1.0-m))*((StanProduction$t^(1.0-m))-1.0))
    results <- coef(lm(StanProduction$q ~ tam ))
    qinf <- results[1]
    qi <- results[2]
    posterior$qinf[k] <- qinf
    posterior$qi[k] <- qi
    
    posterior$EUR[k] <- StanProduction$cumtodate;
    
    t_star <- 30.4375*(1:600)
    if (m==1) {
      tam_star <- t_star^(a-1.0)
    } else {
      tam_star <- (t_star^(-m))*exp((a/(1.0-m))*((t_star^(1.0-m))-1.0))
    }
    q_star <- (qi*tam_star + qinf)[(qi*tam_star + qinf)>StanProduction$qf]
    posterior$EUR[k] <- max(StanProduction$cumtodate, (tail(q_star,1)/a)*t_star[length(q_star)]^m)
  }
  
  #print(head(posterior))
  #flush.console()
  closeAllConnections()
  return(posterior)
}

StanDuongModel15 <- function(x){
  UWI <- names(x[1])
  x2 <- subset(x[[1]],q>0)
  x3 <- tail(head(x2,30),36)
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(diff(c(0,x3$Q))/x3$q),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=0.0,
    Q_prior=0.0
  )
  
  #plot(StanProduction$t, (StanProduction$q/StanProduction$Q), log="xy",xlim=c(1,10000),ylim=c(0.0001,1))
  #flush.console()
  
  DuongResults <- sampling(
    CompiledDuongModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","m","s3")
  )
  
  posterior <- as.data.frame(extract(DuongResults))
  posterior$qinf <- NA
  posterior$qi <- NA
  posterior$EUR <- NA
  
  for(k in 1:length(posterior$a)) {
    #for(k in 1:6) {
    a <- posterior$a[k]
    m <- posterior$m[k]
    tam <- (StanProduction$t^(-m))*exp((a/(1.0-m))*((StanProduction$t^(1.0-m))-1.0))
    results <- coef(lm(StanProduction$q ~ tam ))
    qinf <- results[1]
    qi <- results[2]
    posterior$qinf[k] <- qinf
    posterior$qi[k] <- qi
    
    posterior$EUR[k] <- StanProduction$cumtodate;
    
    t_star <- 30.4375*(1:600)
    if (m==1) {
      tam_star <- t_star^(a-1.0)
    } else {
      tam_star <- (t_star^(-m))*exp((a/(1.0-m))*((t_star^(1.0-m))-1.0))
    }
    q_star <- (qi*tam_star + qinf)[(qi*tam_star + qinf)>StanProduction$qf]
    posterior$EUR[k] <- max(StanProduction$cumtodate, (tail(q_star,1)/a)*t_star[length(q_star)]^m)
  }
  
  #print(head(posterior))
  #flush.console()
  closeAllConnections()
  return(posterior)
}

StanDuongModel18 <- function(x){
  UWI <- names(x[1])
  x2 <- subset(x[[1]],q>0)
  x3 <- tail(head(x2,36),36)
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(diff(c(0,x3$Q))/x3$q),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=0.0,
    Q_prior=0.0
  )
  
  #plot(StanProduction$t, (StanProduction$q/StanProduction$Q), log="xy",xlim=c(1,10000),ylim=c(0.0001,1))
  #flush.console()
  
  DuongResults <- sampling(
    CompiledDuongModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","m","s3")
  )
  
  posterior <- as.data.frame(extract(DuongResults))
  posterior$qinf <- NA
  posterior$qi <- NA
  posterior$EUR <- NA
  
  for(k in 1:length(posterior$a)) {
    #for(k in 1:6) {
    a <- posterior$a[k]
    m <- posterior$m[k]
    tam <- (StanProduction$t^(-m))*exp((a/(1.0-m))*((StanProduction$t^(1.0-m))-1.0))
    results <- coef(lm(StanProduction$q ~ tam ))
    qinf <- results[1]
    qi <- results[2]
    posterior$qinf[k] <- qinf
    posterior$qi[k] <- qi
    
    posterior$EUR[k] <- StanProduction$cumtodate;
    
    t_star <- 30.4375*(1:600)
    if (m==1) {
      tam_star <- t_star^(a-1.0)
    } else {
      tam_star <- (t_star^(-m))*exp((a/(1.0-m))*((t_star^(1.0-m))-1.0))
    }
    q_star <- (qi*tam_star + qinf)[(qi*tam_star + qinf)>StanProduction$qf]
    posterior$EUR[k] <- max(StanProduction$cumtodate, (tail(q_star,1)/a)*t_star[length(q_star)]^m)
  }
  
  #print(head(posterior))
  #flush.console()
  closeAllConnections()
  return(posterior)
}

StanDuongModel21 <- function(x){
  UWI <- names(x[1])
  x2 <- subset(x[[1]],q>0)
  x3 <- tail(head(subset(x[[1]],q>0),39),36)
  if(as.numeric(rownames(x3)[1])>1) {
    t_prior <- cumsum(as.numeric(days_in_month(x2$t)))[as.numeric(rownames(x3)[1])-1]
  } else {
    t_prior <- 0
  }
  if((length(x2[,1])-length(x3[,1]))>0) {
    Q_prior <- x2$Q[as.integer(row.names(x3)[1])-1]
  } else {
    Q_prior <- 0
  }
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(as.numeric(days_in_month(x3$t))),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=t_prior,
    Q_prior=Q_prior
  )
  
  DuongResults <- sampling(
    CompiledDuongModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","m","s3")
  )
  
  posterior <- as.data.frame(extract(DuongResults))
  posterior$qinf <- NA
  posterior$qi <- NA
  posterior$EUR <- NA
  
  for(k in 1:length(posterior$a)) {
    #for(k in 1:6) {
    a <- posterior$a[k]
    m <- posterior$m[k]
    tam <- (StanProduction$t^(-m))*exp((a/(1.0-m))*((StanProduction$t^(1.0-m))-1.0))
    results <- coef(lm(StanProduction$q ~ tam ))
    qinf <- results[1]
    qi <- results[2]
    posterior$qinf[k] <- qinf
    posterior$qi[k] <- qi
    
    posterior$EUR[k] <- StanProduction$cumtodate;
    
    t_star <- 30.4375*(1:600)
    if (m==1) {
      tam_star <- t_star^(a-1.0)
    } else {
      tam_star <- (t_star^(-m))*exp((a/(1.0-m))*((t_star^(1.0-m))-1.0))
    }
    q_star <- (qi*tam_star + qinf)[(qi*tam_star + qinf)>StanProduction$qf]
    posterior$EUR[k] <- max(StanProduction$cumtodate, (tail(q_star,1)/a)*t_star[length(q_star)]^m)
  }
  
  #print(head(posterior))
  #flush.console()
  closeAllConnections()
  return(posterior)
}

StanDuongModel24 <- function(x){
  UWI <- names(x[1])
  x2 <- subset(x[[1]],q>0)
  x3 <- tail(head(x2,42),36)
  if(as.numeric(rownames(x3)[1])>1) {
    t_prior <- cumsum(as.numeric(days_in_month(x2$t)))[as.numeric(rownames(x3)[1])-1]
  } else {
    t_prior <- 0
  }
  if((length(x2[,1])-length(x3[,1]))>0) {
    Q_prior <- x2$Q[as.integer(row.names(x3)[1])-1]
  } else {
    Q_prior <- 0
  }
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(as.numeric(days_in_month(x3$t))),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=t_prior,
    Q_prior=Q_prior
  )
  
  #plot(StanProduction$t, (StanProduction$q/StanProduction$Q), log="xy",xlim=c(1,10000),ylim=c(0.0001,1))
  #flush.console()
  
  DuongResults <- sampling(
    CompiledDuongModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","m","s3")
  )
  
  posterior <- as.data.frame(extract(DuongResults))
  posterior$qinf <- NA
  posterior$qi <- NA
  posterior$EUR <- NA
  
  for(k in 1:length(posterior$a)) {
  #for(k in 1:6) {
    a <- posterior$a[k]
    m <- posterior$m[k]
    tam <- (StanProduction$t^(-m))*exp((a/(1.0-m))*((StanProduction$t^(1.0-m))-1.0))
    results <- coef(lm(StanProduction$q ~ tam ))
    qinf <- results[1]
    qi <- results[2]
    posterior$qinf[k] <- qinf
    posterior$qi[k] <- qi
    
    posterior$EUR[k] <- StanProduction$cumtodate;
    
    t_star <- 30.4375*(1:600)
    if (m==1) {
      tam_star <- t_star^(a-1.0)
    } else {
      tam_star <- (t_star^(-m))*exp((a/(1.0-m))*((t_star^(1.0-m))-1.0))
    }
    q_star <- (qi*tam_star + qinf)[(qi*tam_star + qinf)>StanProduction$qf]
    posterior$EUR[k] <- max(StanProduction$cumtodate, (tail(q_star,1)/a)*t_star[length(q_star)]^m)
  }
  
  #print(head(posterior))
  #flush.console()
  closeAllConnections()
  return(posterior)
}

StanDuongModel36 <- function(x){
  UWI <- names(x[1])
  x2 <- subset(x[[1]],q>0)
  x3 <- tail(head(x2,54),48)
  if(as.numeric(rownames(x3)[1])>1) {
    t_prior <- cumsum(as.numeric(days_in_month(x2$t)))[as.numeric(rownames(x3)[1])-1]
  } else {
    t_prior <- 0
  }
  if((length(x2[,1])-length(x3[,1]))>0) {
    Q_prior <- x2$Q[as.integer(row.names(x3)[1])-1]
  } else {
    Q_prior <- 0
  }
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(as.numeric(days_in_month(x3$t))),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=t_prior,
    Q_prior=Q_prior
  )
  
  #plot(StanProduction$t, (StanProduction$q/StanProduction$Q), log="xy",xlim=c(1,10000),ylim=c(0.0001,1))
  #flush.console()
  
  DuongResults <- sampling(
    CompiledDuongModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","m","s3")
  )
  
  posterior <- as.data.frame(extract(DuongResults))
  posterior$qinf <- NA
  posterior$qi <- NA
  posterior$EUR <- NA
  
  for(k in 1:length(posterior$a)) {
    #for(k in 1:6) {
    a <- posterior$a[k]
    m <- posterior$m[k]
    tam <- (StanProduction$t^(-m))*exp((a/(1.0-m))*((StanProduction$t^(1.0-m))-1.0))
    results <- coef(lm(StanProduction$q ~ tam ))
    qinf <- results[1]
    qi <- results[2]
    posterior$qinf[k] <- qinf
    posterior$qi[k] <- qi
    
    posterior$EUR[k] <- StanProduction$cumtodate;
    
    t_star <- 30.4375*(1:600)
    if (m==1) {
      tam_star <- t_star^(a-1.0)
    } else {
      tam_star <- (t_star^(-m))*exp((a/(1.0-m))*((t_star^(1.0-m))-1.0))
    }
    q_star <- (qi*tam_star + qinf)[(qi*tam_star + qinf)>StanProduction$qf]
    posterior$EUR[k] <- max(StanProduction$cumtodate, (tail(q_star,1)/a)*t_star[length(q_star)]^m)
  }
  
  #print(head(posterior))
  #flush.console()
  closeAllConnections()
  return(posterior)
}

StanDuongModel48 <- function(x){
  UWI <- names(x[1])
  x2 <- subset(x[[1]],q>0)
  x3 <- tail(head(x2,66),60)
  if(as.numeric(rownames(x3)[1])>1) {
    t_prior <- cumsum(as.numeric(days_in_month(x2$t)))[as.numeric(rownames(x3)[1])-1]
  } else {
    t_prior <- 0
  }
  if((length(x2[,1])-length(x3[,1]))>0) {
    Q_prior <- x2$Q[as.integer(row.names(x3)[1])-1]
  } else {
    Q_prior <- 0
  }
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(as.numeric(days_in_month(x3$t))),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=t_prior,
    Q_prior=Q_prior
  )
  
  #plot(StanProduction$t, (StanProduction$q/StanProduction$Q), log="xy",xlim=c(1,10000),ylim=c(0.0001,1))
  #flush.console()
  
  DuongResults <- sampling(
    CompiledDuongModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","m","s3")
  )
  
  posterior <- as.data.frame(extract(DuongResults))
  posterior$qinf <- NA
  posterior$qi <- NA
  posterior$EUR <- NA
  
  for(k in 1:length(posterior$a)) {
    #for(k in 1:6) {
    a <- posterior$a[k]
    m <- posterior$m[k]
    tam <- (StanProduction$t^(-m))*exp((a/(1.0-m))*((StanProduction$t^(1.0-m))-1.0))
    results <- coef(lm(StanProduction$q ~ tam ))
    qinf <- results[1]
    qi <- results[2]
    posterior$qinf[k] <- qinf
    posterior$qi[k] <- qi
    
    posterior$EUR[k] <- StanProduction$cumtodate;
    
    t_star <- 30.4375*(1:600)
    if (m==1) {
      tam_star <- t_star^(a-1.0)
    } else {
      tam_star <- (t_star^(-m))*exp((a/(1.0-m))*((t_star^(1.0-m))-1.0))
    }
    q_star <- (qi*tam_star + qinf)[(qi*tam_star + qinf)>StanProduction$qf]
    posterior$EUR[k] <- max(StanProduction$cumtodate, (tail(q_star,1)/a)*t_star[length(q_star)]^m)
  }
  
  #print(head(posterior))
  #flush.console()
  closeAllConnections()
  return(posterior)
}

StanDuongModel60 <- function(x){
  UWI <- names(x[1])
  x2 <- subset(x[[1]],q>0)
  x3 <- tail(head(x2,78),72)
  if(as.numeric(rownames(x3)[1])>1) {
    t_prior <- cumsum(as.numeric(days_in_month(x2$t)))[as.numeric(rownames(x3)[1])-1]
  } else {
    t_prior <- 0
  }
  if((length(x2[,1])-length(x3[,1]))>0) {
    Q_prior <- x2$Q[as.integer(row.names(x3)[1])-1]
  } else {
    Q_prior <- 0
  }
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(as.numeric(days_in_month(x3$t))),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=t_prior,
    Q_prior=Q_prior
  )
  
  #plot(StanProduction$t, (StanProduction$q/StanProduction$Q), log="xy",xlim=c(1,10000),ylim=c(0.0001,1))
  #flush.console()
  
  DuongResults <- sampling(
    CompiledDuongModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","m","s3")
  )
  
  posterior <- as.data.frame(extract(DuongResults))
  posterior$qinf <- NA
  posterior$qi <- NA
  posterior$EUR <- NA
  
  for(k in 1:length(posterior$a)) {
    #for(k in 1:6) {
    a <- posterior$a[k]
    m <- posterior$m[k]
    tam <- (StanProduction$t^(-m))*exp((a/(1.0-m))*((StanProduction$t^(1.0-m))-1.0))
    results <- coef(lm(StanProduction$q ~ tam ))
    qinf <- results[1]
    qi <- results[2]
    posterior$qinf[k] <- qinf
    posterior$qi[k] <- qi
    
    posterior$EUR[k] <- StanProduction$cumtodate;
    
    t_star <- 30.4375*(1:600)
    if (m==1) {
      tam_star <- t_star^(a-1.0)
    } else {
      tam_star <- (t_star^(-m))*exp((a/(1.0-m))*((t_star^(1.0-m))-1.0))
    }
    q_star <- (qi*tam_star + qinf)[(qi*tam_star + qinf)>StanProduction$qf]
    posterior$EUR[k] <- max(StanProduction$cumtodate, (tail(q_star,1)/a)*t_star[length(q_star)]^m)
  }
  
  #print(head(posterior))
  #flush.console()
  closeAllConnections()
  return(posterior)
}



i<-5
#plot(log(q)~t,data=x[[i]],main=i)
plot(q~Q,data=x[[i]],main=i)
temp <- StanDuongModel24(x[i])
hist(temp$a)


x_months <- unlist(lapply(1:length(x), function(i) length(subset(x[[i]],q>0)$t)))
Duong_03 <- lapply((1:length(x))[x_months>3], function(i) StanDuongModel03(x[i]))
saveRDS(Duong_03, paste(ResourcePlay, "_Duong_03.rds", sep=""))
rm(Duong_03)
gc()
Duong_06 <- lapply((1:length(x))[x_months>6], function(i) StanDuongModel06(x[i]))
saveRDS(Duong_06, paste(ResourcePlay, "_Duong_06.rds", sep=""))
rm(Duong_06)
gc()
Duong_09 <- lapply((1:length(x))[x_months>9], function(i) StanDuongModel09(x[i]))
saveRDS(Duong_09, paste(ResourcePlay, "_Duong_09.rds", sep=""))
rm(Duong_09)
gc()
Duong_12 <- lapply((1:length(x))[x_months>12], function(i) StanDuongModel12(x[i]))
saveRDS(Duong_12, paste(ResourcePlay, "_Duong_12.rds", sep=""))
rm(Duong_12)
gc()
Duong_15 <- lapply((1:length(x))[x_months>15], function(i) StanDuongModel15(x[i]))
saveRDS(Duong_15, paste(ResourcePlay, "_Duong_15.rds", sep=""))
rm(Duong_15)
gc()
Duong_18 <- lapply((1:length(x))[x_months>18], function(i) StanDuongModel18(x[i]))
saveRDS(Duong_18, paste(ResourcePlay, "_Duong_18.rds", sep=""))
rm(Duong_18)
gc()
Duong_21 <- lapply((1:length(x))[x_months>21], function(i) StanDuongModel21(x[i]))
saveRDS(Duong_21, paste(ResourcePlay, "_Duong_21.rds", sep=""))
rm(Duong_21)
gc()
Duong_24 <- lapply((1:length(x))[x_months>24], function(i) StanDuongModel24(x[i]))
saveRDS(Duong_24, paste(ResourcePlay, "_Duong_24.rds", sep=""))
rm(Duong_24)
gc()
Duong_36 <- lapply((1:length(x))[x_months>36], function(i) StanDuongModel36(x[i]))
saveRDS(Duong_36, paste(ResourcePlay, "_Duong_36.rds", sep=""))
rm(Duong_36)
gc()
Duong_48 <- lapply((1:length(x))[x_months>48], function(i) StanDuongModel48(x[i]))
saveRDS(Duong_48, paste(ResourcePlay, "_Duong_48.rds", sep=""))
rm(Duong_48)
gc()
Duong_60 <- lapply((1:length(x))[x_months>60], function(i) StanDuongModel60(x[i]))
saveRDS(Duong_60, paste(ResourcePlay, "_Duong_60.rds", sep=""))
rm(Duong_60)
gc()


if(FALSE) {
  Duong_03 <- lapply(na.omit((1:100)[x_months>3]), function(i) StanDuongModel03(x[i]))
  Duong_06 <- lapply(na.omit((1:100)[x_months>6]), function(i) StanDuongModel06(x[i]))
  Duong_09 <- lapply(na.omit((1:100)[x_months>9]), function(i) StanDuongModel09(x[i]))
  Duong_12 <- lapply(na.omit((1:100)[x_months>12]), function(i) StanDuongModel12(x[i]))
  Duong_15 <- lapply(na.omit((1:100)[x_months>15]), function(i) StanDuongModel15(x[i]))
  Duong_18 <- lapply(na.omit((1:100)[x_months>18]), function(i) StanDuongModel18(x[i]))
  Duong_21 <- lapply(na.omit((1:100)[x_months>21]), function(i) StanDuongModel21(x[i]))
  Duong_24 <- lapply(na.omit((1:100)[x_months>24]), function(i) StanDuongModel24(x[i]))
}

if(FALSE) {
  Duong_03 <- lapply(na.omit((1:length(x))[x_months>3]), function(i) StanDuongModel03(x[i]))
  Duong_06 <- lapply(na.omit((1:length(x))[x_months>6]), function(i) StanDuongModel06(x[i]))
  Duong_09 <- lapply(na.omit((1:length(x))[x_months>9]), function(i) StanDuongModel09(x[i]))
  Duong_12 <- lapply(na.omit((1:length(x))[x_months>12]), function(i) StanDuongModel12(x[i]))
  Duong_15 <- lapply(na.omit((1:length(x))[x_months>15]), function(i) StanDuongModel15(x[i]))
  Duong_18 <- lapply(na.omit((1:length(x))[x_months>18]), function(i) StanDuongModel18(x[i]))
  Duong_21 <- lapply(na.omit((1:length(x))[x_months>21]), function(i) StanDuongModel21(x[i]))
  Duong_24 <- lapply(na.omit((1:length(x))[x_months>24]), function(i) StanDuongModel24(x[i]))
}

if(FALSE) {
  Duong_03 <- lapply(na.omit((1:length(x))[x_months>24]), function(i) StanDuongModel03(x[i]))
  Duong_06 <- lapply(na.omit((1:length(x))[x_months>24]), function(i) StanDuongModel06(x[i]))
  Duong_09 <- lapply(na.omit((1:length(x))[x_months>24]), function(i) StanDuongModel09(x[i]))
  Duong_12 <- lapply(na.omit((1:length(x))[x_months>24]), function(i) StanDuongModel12(x[i]))
  Duong_15 <- lapply(na.omit((1:length(x))[x_months>24]), function(i) StanDuongModel15(x[i]))
  Duong_18 <- lapply(na.omit((1:length(x))[x_months>24]), function(i) StanDuongModel18(x[i]))
  Duong_21 <- lapply(na.omit((1:length(x))[x_months>24]), function(i) StanDuongModel21(x[i]))
  Duong_24 <- lapply(na.omit((1:length(x))[x_months>24]), function(i) StanDuongModel24(x[i]))
}




Duong_03 <- readRDS(paste(ResourcePlay, "_Duong_03.rds", sep=""))
a_03 <- lapply(1:length(Duong_03), function(i) Duong_03[[i]]$a)
s3_03 <- lapply(1:length(Duong_03), function(i) Duong_03[[i]]$s3)
rm(Duong_03)
gc()
Duong_06 <- readRDS(paste(ResourcePlay, "_Duong_06.rds", sep=""))
a_06 <- lapply(1:length(Duong_06), function(i) Duong_06[[i]]$a)
s3_06 <- lapply(1:length(Duong_06), function(i) Duong_06[[i]]$s3)
rm(Duong_06)
gc()
Duong_09 <- readRDS(paste(ResourcePlay, "_Duong_09.rds", sep=""))
a_09 <- lapply(1:length(Duong_09), function(i) Duong_09[[i]]$a)
s3_09 <- lapply(1:length(Duong_09), function(i) Duong_09[[i]]$s3)
rm(Duong_09)
gc()
Duong_12 <- readRDS(paste(ResourcePlay, "_Duong_12.rds", sep=""))
a_12 <- lapply(1:length(Duong_12), function(i) Duong_12[[i]]$a)
s3_12 <- lapply(1:length(Duong_12), function(i) Duong_12[[i]]$s2)
rm(Duong_12)
gc()
Duong_15 <- readRDS(paste(ResourcePlay, "_Duong_15.rds", sep=""))
a_15 <- lapply(1:length(Duong_15), function(i) Duong_15[[i]]$a)
s3_15 <- lapply(1:length(Duong_15), function(i) Duong_15[[i]]$s3)
rm(Duong_15)
gc()
Duong_18 <- readRDS(paste(ResourcePlay, "_Duong_18.rds", sep=""))
a_18 <- lapply(1:length(Duong_18), function(i) Duong_18[[i]]$a)
s3_18 <- lapply(1:length(Duong_18), function(i) Duong_18[[i]]$s3)
rm(Duong_18)
gc()
Duong_21 <- readRDS(paste(ResourcePlay, "_Duong_21.rds", sep=""))
a_21 <- lapply(1:length(Duong_21), function(i) Duong_21[[i]]$a)
s3_21 <- lapply(1:length(Duong_21), function(i) Duong_21[[i]]$s3)
rm(Duong_21)
gc()
Duong_24 <- readRDS(paste(ResourcePlay, "_Duong_24.rds", sep=""))
a_24 <- lapply(1:length(Duong_24), function(i) Duong_24[[i]]$a)
s3_24 <- lapply(1:length(Duong_24), function(i) Duong_24[[i]]$s3)
rm(Duong_24)
gc()
Duong_36 <- readRDS(paste(ResourcePlay, "_Duong_36.rds", sep=""))
a_36 <- lapply(1:length(Duong_36), function(i) Duong_36[[i]]$a)
s3_36 <- lapply(1:length(Duong_36), function(i) Duong_36[[i]]$s3)
rm(Duong_36)
gc()
Duong_48 <- readRDS(paste(ResourcePlay, "_Duong_48.rds", sep=""))
a_48 <- lapply(1:length(Duong_48), function(i) Duong_48[[i]]$a)
s3_48 <- lapply(1:length(Duong_48), function(i) Duong_48[[i]]$s3)
rm(Duong_48)
gc()
Duong_60 <- readRDS(paste(ResourcePlay, "_Duong_60.rds", sep=""))
a_60 <- lapply(1:length(Duong_60), function(i) Duong_60[[i]]$a)
s3_60 <- lapply(1:length(Duong_60), function(i) Duong_60[[i]]$s3)
rm(Duong_60)
gc()


if(FALSE) {
  hist(unlist(a_03),breaks=100)
  hist(unlist(a_06),breaks=100)
  hist(unlist(a_09),breaks=100)
  hist(unlist(a_12),breaks=100)
  hist(unlist(a_15),breaks=100)
  hist(unlist(a_18),breaks=100)
  hist(unlist(a_21),breaks=100)
  hist(unlist(a_24),breaks=100)
  hist(unlist(a_36),breaks=100)
  hist(unlist(a_48),breaks=100)
  hist(unlist(a_60),breaks=100)
}



#let's build a sample hist for one well's changes over time
for(i in 1:length(Duong_03)) {
#i<-2
#plot(log(q)~t,data=x[[i]],main=i)
#plot(q~Q,data=x[[i]],main=i)
sample_well <- rbind(
  data.frame(EUR=Duong_03[[i]]$EUR, Months="3"),
  data.frame(EUR=Duong_06[[i]]$EUR, Months="6"),
  data.frame(EUR=Duong_09[[i]]$EUR, Months="9"),
  data.frame(EUR=Duong_12[[i]]$EUR, Months="12"),
  data.frame(EUR=Duong_15[[i]]$EUR, Months="15"),
  data.frame(EUR=Duong_18[[i]]$EUR, Months="18")
)
ggplot(sample_well, aes(EUR, fill = Months)) + geom_density(alpha = 0.2) + scale_x_continuous(limits = c(0, 10e6)) + ggtitle(names(x[i]))
#ggplot(sample_well, aes(EUR, fill = Months)) + geom_density(alpha = 0.2) + ggtitle(names(x[i]))
ggsave(filename=paste(gsub("[[:punct:]]", "", names(x[i])),".pdf",sep=""))
}

