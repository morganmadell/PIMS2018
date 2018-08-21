#' @import rstan

detachPackage("snowfall")
detachPackage("doSNOW")
detachPackage("snow")
library(rstan)
library(parallel)
options(mc.cores = parallel::detectCores())

CompiledSepdModel <- stan_model(file=paste(workdir, "R/", "SepdModel.stan", sep=""), auto_write=TRUE)

StanSepdModel <- function(x){
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
  
  SepdResults <- sampling(
    CompiledSepdModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","tau","n","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(SepdResults))
}

StanSepdModel03 <- function(x){
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
  
  SepdResults <- sampling(
    CompiledSepdModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","tau","n","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(SepdResults))
}

StanSepdModel06 <- function(x){
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
  
  SepdResults <- sampling(
    CompiledSepdModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","tau","n","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(SepdResults))
}

StanSepdModel09 <- function(x){
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
  
  SepdResults <- sampling(
    CompiledSepdModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","tau","n","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(SepdResults))
}

StanSepdModel12 <- function(x){
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
  
  SepdResults <- sampling(
    CompiledSepdModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","tau","n","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(SepdResults))
}

StanSepdModel15 <- function(x){
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
  
  SepdResults <- sampling(
    CompiledSepdModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","tau","n","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(SepdResults))
}

StanSepdModel18 <- function(x){
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
  
  SepdResults <- sampling(
    CompiledSepdModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","tau","n","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(SepdResults))
}

StanSepdModel21 <- function(x){
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
  #t_prior=cumsum(as.numeric(days_in_month(x2$t)))[3],
  #Q_prior=x2$Q[3]
  
  SepdResults <- sampling(
    CompiledSepdModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","tau","n","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(SepdResults))
}

StanSepdModel24 <- function(x){
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
  
  SepdResults <- sampling(
    chains=1,
    CompiledSepdModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","tau","n","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(SepdResults))
}

StanSepdModel36 <- function(x){
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
  
  SepdResults <- sampling(
    CompiledSepdModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","tau","n","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(SepdResults))
}

StanSepdModel48 <- function(x){
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
  
  SepdResults <- sampling(
    CompiledSepdModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","tau","n","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(SepdResults))
}

StanSepdModel60 <- function(x){
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
  
  SepdResults <- sampling(
    CompiledSepdModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","tau","n","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(SepdResults))
}




#SepdResults <- stan(
#  model_code = SepdModel,
#  data = StanProduction,
#  iter = 2000
#)
#hist(extract(SepdResults)$EUR,breaks=100)
#hist(extract(SepdResults)$Di,breaks=100)
#hist(extract(SepdResults)$qi,breaks=100)

i<-9
#plot(log(q)~t,data=x[[i]],main=i)
plot(q~Q,data=x[[i]],main=i)
temp <- StanSepdModel21(x[i])
hist(temp$tau)


#sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
#sfInit(parallel=FALSE, cpus=1, type="SOCK", socketHosts=rep('localhost',CPU_count) )

#sfExport("StanSepdModel3","CompiledSepdModel", "x")
#sfExport("StanSepdModel12","CompiledSepdModel", "x")
#sfLibrary(lubridate)
#sfLibrary(rstan)
#x_months <- unlist(sfClusterApplyLB(1:length(x), function(i) length(x[[i]]$t)))
#Sepd_3 <- sfClusterApplyLB((1:length(x))[x_months>3], function(i) StanSepdModel3(x[i]))
#Sepd_6 <- sfClusterApplyLB((1:length(x))[x_months>6], function(i) StanSepdModel6(x[i]))
#Sepd_9 <- sfClusterApplyLB((1:length(x))[x_months>9], function(i) StanSepdModel9(x[i]))
#Sepd_12 <- sfClusterApplyLB((1:length(x))[x_months>12], function(i) StanSepdModel12(x[i]))
#Sepd_15 <- sfClusterApplyLB((1:length(x))[x_months>15], function(i) StanSepdModel15(x[i]))
#Sepd_18 <- sfClusterApplyLB((1:length(x))[x_months>18], function(i) StanSepdModel18(x[i]))
#Sepd_21 <- sfClusterApplyLB((1:length(x))[x_months>21], function(i) StanSepdModel21(x[i]))
#Sepd_24 <- sfClusterApplyLB((1:length(x))[x_months>24], function(i) StanSepdModel24(x[i]))
#temp <- sfClusterApplyLB(1:10, function(i) StanSepdModelMoving(x[i]))

x_months <- unlist(lapply(1:length(x), function(i) length(x[[i]]$t)))
Sepd_03 <- lapply((1:length(x))[x_months>3], function(i) StanSepdModel03(x[i]))
saveRDS(Sepd_03, paste(ResourcePlay, "_Sepd_03.rds", sep=""))
rm(Sepd_03)
gc()
Sepd_06 <- lapply((1:length(x))[x_months>6], function(i) StanSepdModel06(x[i]))
saveRDS(Sepd_06, paste(ResourcePlay, "_Sepd_06.rds", sep=""))
rm(Sepd_06)
gc()
Sepd_09 <- lapply((1:length(x))[x_months>9], function(i) StanSepdModel09(x[i]))
saveRDS(Sepd_09, paste(ResourcePlay, "_Sepd_09.rds", sep=""))
rm(Sepd_09)
gc()
Sepd_12 <- lapply((1:length(x))[x_months>12], function(i) StanSepdModel12(x[i]))
saveRDS(Sepd_12, paste(ResourcePlay, "_Sepd_12.rds", sep=""))
rm(Sepd_12)
gc()
Sepd_15 <- lapply((1:length(x))[x_months>15], function(i) StanSepdModel15(x[i]))
saveRDS(Sepd_15, paste(ResourcePlay, "_Sepd_15.rds", sep=""))
rm(Sepd_15)
Sepd_18 <- lapply((1:length(x))[x_months>18], function(i) StanSepdModel18(x[i]))
saveRDS(Sepd_18, paste(ResourcePlay, "_Sepd_18.rds", sep=""))
rm(Sepd_18)
Sepd_21 <- lapply((1:length(x))[x_months>21], function(i) StanSepdModel21(x[i]))
saveRDS(Sepd_21, paste(ResourcePlay, "_Sepd_21.rds", sep=""))
rm(Sepd_21)
gc()
Sepd_24 <- lapply((1:length(x))[x_months>24], function(i) StanSepdModel24(x[i]))
saveRDS(Sepd_24, paste(ResourcePlay, "_Sepd_24.rds", sep=""))
rm(Sepd_24)
gc()
Sepd_36 <- lapply((1:length(x))[x_months>36], function(i) StanSepdModel36(x[i]))
saveRDS(Sepd_36, paste(ResourcePlay, "_Sepd_36.rds", sep=""))
rm(Sepd_36)
gc()
Sepd_48 <- lapply((1:length(x))[x_months>48], function(i) StanSepdModel48(x[i]))
saveRDS(Sepd_48, paste(ResourcePlay, "_Sepd_48.rds", sep=""))
rm(Sepd_48)
gc()
Sepd_60 <- lapply((1:length(x))[x_months>60], function(i) StanSepdModel60(x[i]))
saveRDS(Sepd_60, paste(ResourcePlay, "_Sepd_60.rds", sep=""))
rm(Sepd_60)
gc()
