#' @import rstan

detachPackage("snowfall")
detachPackage("doSNOW")
detachPackage("snow")
library(rstan)
library(parallel)
options(mc.cores = parallel::detectCores())

CompiledPleModel <- stan_model(file=paste(workdir, "R/", "PleModel.stan", sep=""),auto_write=TRUE)

StanPleModel <- function(x){
  UWI <- names(x[1])
  x3 <- subset(x[[1]],q>0)
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(as.numeric(days_in_month(x3$t))),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=0.0,
    Q_prior=0.0
  )
  
  PleResults <- sampling(
    CompiledPleModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","Dinf","n","b","b_mid","s2")
  )
  
  closeAllConnections()
  return(as.data.frame(PleResults))
}

StanPleModel03 <- function(x){
  UWI <- names(x[1])
  x3 <- tail(head(subset(x[[1]],q>0),6),36)
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(as.numeric(days_in_month(x3$t))),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=0.0,
    Q_prior=0.0
  )
  
  PleResults <- sampling(
    CompiledPleModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","Dinf","n","b","b_mid","s2")
  )
  #algorithm="HMC",
  #chains = 5,
  #control=list(adapt_t0=20, adapt_delta=0.975,max_treedepth=30),
  
  closeAllConnections()
  return(as.data.frame(PleResults))
}

StanPleModel06 <- function(x){
  UWI <- names(x[1])
  x3 <- tail(head(subset(x[[1]],q>0),12),36)
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(as.numeric(days_in_month(x3$t))),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=0.0,
    Q_prior=0.0
  )
  
  PleResults <- sampling(
    CompiledPleModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","Dinf","n","b","b_mid","s2")
  )
  
  closeAllConnections()
  return(as.data.frame(PleResults))
}

StanPleModel09 <- function(x){
  UWI <- names(x[1])
  x3 <- tail(head(subset(x[[1]],q>0),18),36)
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(as.numeric(days_in_month(x3$t))),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=0.0,
    Q_prior=0.0
  )
  
  PleResults <- sampling(
    CompiledPleModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","Dinf","n","b","b_mid","s2")
  )
  
  closeAllConnections()
  return(as.data.frame(PleResults))
}

StanPleModel12 <- function(x){
  UWI <- names(x[1])
  x3 <- tail(head(subset(x[[1]],q>0),24),36)
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(as.numeric(days_in_month(x3$t))),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=0.0,
    Q_prior=0.0
  )
  
  PleResults <- sampling(
    CompiledPleModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","Dinf","n","b","b_mid","s2")
  )
  
  closeAllConnections()
  return(as.data.frame(PleResults))
}

StanPleModel15 <- function(x){
  UWI <- names(x[1])
  x3 <- tail(head(subset(x[[1]],q>0),30),36)
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(as.numeric(days_in_month(x3$t))),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=0.0,
    Q_prior=0.0
  )
  
  PleResults <- sampling(
    CompiledPleModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","Dinf","n","b","b_mid","s2")
  )
  
  closeAllConnections()
  return(as.data.frame(PleResults))
}

StanPleModel18 <- function(x){
  UWI <- names(x[1])
  x3 <- tail(head(subset(x[[1]],q>0),36),36)
  StanProduction <- list(
    N=nrow(x3),
    t=cumsum(as.numeric(days_in_month(x3$t))),
    q=x3$q,
    Q=x3$Q,
    maxrate=max(x3$q),
    cumtodate=max(x3$Q),
    qf=max(x3$qf),
    t_prior=0.0,
    Q_prior=0.0
  )
  
  PleResults <- sampling(
    CompiledPleModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","Dinf","n","b","b_mid","s2")
  )
  
  closeAllConnections()
  return(as.data.frame(PleResults))
}

StanPleModel21 <- function(x){
  UWI <- names(x[1])
  x2 <- subset(x[[1]],q>0)
  x3 <- tail(head(x2,39),36)
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
  
  PleResults <- sampling(
    CompiledPleModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","Dinf","n","b","b_mid","s2")
  )
  
  closeAllConnections()
  return(as.data.frame(PleResults))
}

StanPleModel24 <- function(x){
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
  
  PleResults <- sampling(
    CompiledPleModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","Dinf","n","b","b_mid","s2")
  )
  
  closeAllConnections()
  return(as.data.frame(PleResults))
}

StanPleModel36 <- function(x){
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
  
  PleResults <- sampling(
    CompiledPleModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","Dinf","n","b","b_mid","s2")
  )
  
  closeAllConnections()
  return(as.data.frame(PleResults))
}

StanPleModel48 <- function(x){
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
  
  PleResults <- sampling(
    CompiledPleModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","Dinf","n","b","b_mid","s2")
  )
  
  closeAllConnections()
  return(as.data.frame(PleResults))
}

StanPleModel60 <- function(x){
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
  
  PleResults <- sampling(
    CompiledPleModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","Dinf","n","b","b_mid","s2")
  )
  
  closeAllConnections()
  return(as.data.frame(PleResults))
}



i<-9
#plot(log(q)~t,data=x[[i]],main=i)
plot(q~Q,data=x[[i]],main=i)
temp <- StanPleModel21(x[i])
hist(temp$b_mid)
hist(temp$b)



x_months <- unlist(lapply(1:length(x), function(i) length(x[[i]]$t)))
Ple_03 <- lapply((1:length(x))[x_months>3], function(i) StanPleModel03(x[i]))
saveRDS(Ple_03, paste(ResourcePlay, "_Ple_03.rds", sep=""))
rm(Ple_03)
gc()
Ple_06 <- lapply((1:length(x))[x_months>6], function(i) StanPleModel06(x[i]))
saveRDS(Ple_06, paste(ResourcePlay, "_Ple_06.rds", sep=""))
rm(Ple_06)
gc()
Ple_09 <- lapply((1:length(x))[x_months>9], function(i) StanPleModel09(x[i]))
saveRDS(Ple_09, paste(ResourcePlay, "_Ple_09.rds", sep=""))
rm(Ple_09)
gc()
Ple_12 <- lapply((1:length(x))[x_months>12], function(i) StanPleModel12(x[i]))
saveRDS(Ple_12, paste(ResourcePlay, "_Ple_12.rds", sep=""))
rm(Ple_12)
gc()
Ple_15 <- lapply((1:length(x))[x_months>15], function(i) StanPleModel15(x[i]))
saveRDS(Ple_15, paste(ResourcePlay, "_Ple_15.rds", sep=""))
rm(Ple_15)
Ple_18 <- lapply((1:length(x))[x_months>18], function(i) StanPleModel18(x[i]))
saveRDS(Ple_18, paste(ResourcePlay, "_Ple_18.rds", sep=""))
rm(Ple_18)
Ple_21 <- lapply((1:length(x))[x_months>21], function(i) StanPleModel21(x[i]))
saveRDS(Ple_21, paste(ResourcePlay, "_Ple_21.rds", sep=""))
rm(Ple_21)
gc()
Ple_24 <- lapply((1:length(x))[x_months>24], function(i) StanPleModel24(x[i]))
saveRDS(Ple_24, paste(ResourcePlay, "_Ple_24.rds", sep=""))
rm(Ple_24)
gc()
Ple_36 <- lapply((1:length(x))[x_months>36], function(i) StanPleModel36(x[i]))
saveRDS(Ple_36, paste(ResourcePlay, "_Ple_36.rds", sep=""))
rm(Ple_36)
gc()
Ple_48 <- lapply((1:length(x))[x_months>48], function(i) StanPleModel48(x[i]))
saveRDS(Ple_48, paste(ResourcePlay, "_Ple_48.rds", sep=""))
rm(Ple_48)
gc()
Ple_60 <- lapply((1:length(x))[x_months>60], function(i) StanPleModel60(x[i]))
saveRDS(Ple_60, paste(ResourcePlay, "_Ple_60.rds", sep=""))
rm(Ple_60)
gc()

