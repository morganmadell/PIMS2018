#' @import rstan

detachPackage("snowfall")
detachPackage("doSNOW")
detachPackage("snow")
library(rstan)
library(parallel)
options(mc.cores = parallel::detectCores())
#options(mc.cores = 1)
#control=list(adapt_delta=0.9)

#StanArpsResults <- stan(
#  model_code = ArpsModel,
#  data = StanProduction,
#  iter = iters
#)

CompiledArpsModel <- stan_model(file=paste(workdir, "R/", "ArpsModel.stan", sep=""), auto_write=TRUE)
#CompiledArpsModel <- stan_model(file=paste(workdir, "R/", "ArpsModel.stan", sep=""),auto_write=FALSE)

StanArpsModel <- function(x){
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
  
  ArpsResults <- sampling(
    CompiledArpsModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","b","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(ArpsResults)))
}

StanArpsModel03 <- function(x){
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
  
  ArpsResults <- sampling(
    CompiledArpsModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","b","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(ArpsResults)))
}

StanArpsModel06 <- function(x){
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
  
  ArpsResults <- sampling(
    CompiledArpsModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","b","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(ArpsResults)))
}

StanArpsModel09 <- function(x){
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
  
  ArpsResults <- sampling(
    CompiledArpsModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","b","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(ArpsResults)))
}

StanArpsModel12 <- function(x){
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
  
  ArpsResults <- sampling(
    CompiledArpsModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","b","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(ArpsResults)))
}

StanArpsModel15 <- function(x){
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
  
  ArpsResults <- sampling(
    CompiledArpsModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","b","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(ArpsResults)))
}

StanArpsModel18 <- function(x){
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
  
  ArpsResults <- sampling(
    CompiledArpsModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","b","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(ArpsResults)))
}

StanArpsModel21 <- function(x){
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
  
  ArpsResults <- sampling(
    CompiledArpsModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","b","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(ArpsResults)))
}

StanArpsModel24 <- function(x){
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
  
  ArpsResults <- sampling(
    chains=1,
    CompiledArpsModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","b","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(ArpsResults)))
}

StanArpsModel36 <- function(x){
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
  
  ArpsResults <- sampling(
    CompiledArpsModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","b","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(ArpsResults)))
}

StanArpsModel48 <- function(x){
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
  
  ArpsResults <- sampling(
    CompiledArpsModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","b","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(ArpsResults)))
}

StanArpsModel60 <- function(x){
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
  
  ArpsResults <- sampling(
    CompiledArpsModel, 
    data=StanProduction,
    iter = iters,
    pars=c("qi","Di","b","s2","EUR")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(ArpsResults)))
}




#ArpsResults <- stan(
#  model_code = ArpsModel,
#  data = StanProduction,
#  iter = 2000
#)
#hist(extract(ArpsResults)$EUR,breaks=100)
#hist(extract(ArpsResults)$Di,breaks=100)
#hist(extract(ArpsResults)$qi,breaks=100)

i<-9
#plot(log(q)~t,data=x[[i]],main=i)
plot(q~Q,data=x[[i]],main=i)
temp <- StanArpsModel21(x[i])
hist(temp$b)


#sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
#sfInit(parallel=FALSE, cpus=1, type="SOCK", socketHosts=rep('localhost',CPU_count) )

#sfExport("StanArpsModel3","CompiledArpsModel", "x")
#sfExport("StanArpsModel12","CompiledArpsModel", "x")
#sfLibrary(lubridate)
#sfLibrary(rstan)
#x_months <- unlist(sfClusterApplyLB(1:length(x), function(i) length(x[[i]]$t)))
#Arps_3 <- sfClusterApplyLB((1:length(x))[x_months>3], function(i) StanArpsModel3(x[i]))
#Arps_6 <- sfClusterApplyLB((1:length(x))[x_months>6], function(i) StanArpsModel6(x[i]))
#Arps_9 <- sfClusterApplyLB((1:length(x))[x_months>9], function(i) StanArpsModel9(x[i]))
#Arps_12 <- sfClusterApplyLB((1:length(x))[x_months>12], function(i) StanArpsModel12(x[i]))
#Arps_15 <- sfClusterApplyLB((1:length(x))[x_months>15], function(i) StanArpsModel15(x[i]))
#Arps_18 <- sfClusterApplyLB((1:length(x))[x_months>18], function(i) StanArpsModel18(x[i]))
#Arps_21 <- sfClusterApplyLB((1:length(x))[x_months>21], function(i) StanArpsModel21(x[i]))
#Arps_24 <- sfClusterApplyLB((1:length(x))[x_months>24], function(i) StanArpsModel24(x[i]))
#temp <- sfClusterApplyLB(1:10, function(i) StanArpsModelMoving(x[i]))

x_months <- unlist(lapply(1:length(x), function(i) length(x[[i]]$t)))
saveRDS(x_months, paste(ResourcePlay, "_x_months.rds", sep=""))
saveRDS(names(x[1:length(x)]), paste(ResourcePlay, "_names.rds", sep=""))
saveRDS(names(x[(1:length(x))[x_months>3]]), paste(ResourcePlay, "_names_03.rds", sep=""))
saveRDS(names(x[(1:length(x))[x_months>6]]), paste(ResourcePlay, "_names_06.rds", sep=""))
saveRDS(names(x[(1:length(x))[x_months>9]]), paste(ResourcePlay, "_names_09.rds", sep=""))
saveRDS(names(x[(1:length(x))[x_months>12]]), paste(ResourcePlay, "_names_12.rds", sep=""))
saveRDS(names(x[(1:length(x))[x_months>15]]), paste(ResourcePlay, "_names_15.rds", sep=""))
saveRDS(names(x[(1:length(x))[x_months>18]]), paste(ResourcePlay, "_names_18.rds", sep=""))
saveRDS(names(x[(1:length(x))[x_months>21]]), paste(ResourcePlay, "_names_21.rds", sep=""))
saveRDS(names(x[(1:length(x))[x_months>24]]), paste(ResourcePlay, "_names_24.rds", sep=""))
saveRDS(names(x[(1:length(x))[x_months>36]]), paste(ResourcePlay, "_names_36.rds", sep=""))
saveRDS(names(x[(1:length(x))[x_months>48]]), paste(ResourcePlay, "_names_48.rds", sep=""))
saveRDS(names(x[(1:length(x))[x_months>60]]), paste(ResourcePlay, "_names_60.rds", sep=""))
Arps_03 <- lapply((1:length(x))[x_months>3], function(i) StanArpsModel03(x[i]))
saveRDS(Arps_03, paste(ResourcePlay, "_Arps_03.rds", sep=""))
rm(Arps_03)
gc()
Arps_06 <- lapply((1:length(x))[x_months>6], function(i) StanArpsModel06(x[i]))
saveRDS(Arps_06, paste(ResourcePlay, "_Arps_06.rds", sep=""))
rm(Arps_06)
gc()
Arps_09 <- lapply((1:length(x))[x_months>9], function(i) StanArpsModel09(x[i]))
saveRDS(Arps_09, paste(ResourcePlay, "_Arps_09.rds", sep=""))
rm(Arps_09)
gc()
Arps_12 <- lapply((1:length(x))[x_months>12], function(i) StanArpsModel12(x[i]))
saveRDS(Arps_12, paste(ResourcePlay, "_Arps_12.rds", sep=""))
rm(Arps_12)
gc()
Arps_15 <- lapply((1:length(x))[x_months>15], function(i) StanArpsModel15(x[i]))
saveRDS(Arps_15, paste(ResourcePlay, "_Arps_15.rds", sep=""))
rm(Arps_15)
Arps_18 <- lapply((1:length(x))[x_months>18], function(i) StanArpsModel18(x[i]))
saveRDS(Arps_18, paste(ResourcePlay, "_Arps_18.rds", sep=""))
rm(Arps_18)
Arps_21 <- lapply((1:length(x))[x_months>21], function(i) StanArpsModel21(x[i]))
saveRDS(Arps_21, paste(ResourcePlay, "_Arps_21.rds", sep=""))
rm(Arps_21)
gc()
Arps_24 <- lapply((1:length(x))[x_months>24], function(i) StanArpsModel24(x[i]))
saveRDS(Arps_24, paste(ResourcePlay, "_Arps_24.rds", sep=""))
rm(Arps_24)
gc()
Arps_36 <- lapply((1:length(x))[x_months>36], function(i) StanArpsModel36(x[i]))
saveRDS(Arps_36, paste(ResourcePlay, "_Arps_36.rds", sep=""))
rm(Arps_36)
gc()
Arps_48 <- lapply((1:length(x))[x_months>48], function(i) StanArpsModel48(x[i]))
saveRDS(Arps_48, paste(ResourcePlay, "_Arps_48.rds", sep=""))
rm(Arps_48)
gc()
Arps_60 <- lapply((1:length(x))[x_months>60], function(i) StanArpsModel60(x[i]))
saveRDS(Arps_60, paste(ResourcePlay, "_Arps_60.rds", sep=""))
rm(Arps_60)
gc()

if(FALSE) {
  Arps_03 <- mclapply((1:length(x))[x_months>3], function(i) StanArpsModel3(x[i]))
  Arps_06 <- mclapply((1:length(x))[x_months>6], function(i) StanArpsModel6(x[i]))
  Arps_09 <- mclapply((1:length(x))[x_months>9], function(i) StanArpsModel9(x[i]))
  Arps_12 <- mclapply((1:length(x))[x_months>12], function(i) StanArpsModel12(x[i]))
  Arps_15 <- mclapply((1:length(x))[x_months>15], function(i) StanArpsModel15(x[i]))
  Arps_18 <- mclapply((1:length(x))[x_months>18], function(i) StanArpsModel18(x[i]))
  Arps_21 <- mclapply((1:length(x))[x_months>21], function(i) StanArpsModel21(x[i]))
  Arps_24 <- mclapply((1:length(x))[x_months>24], function(i) StanArpsModel24(x[i]))
  Arps_36 <- mclapply((1:length(x))[x_months>36], function(i) StanArpsModel36(x[i]))
  Arps_48 <- mclapply((1:length(x))[x_months>48], function(i) StanArpsModel48(x[i]))
  Arps_60 <- mclapply((1:length(x))[x_months>60], function(i) StanArpsModel60(x[i]))
}

if(FALSE) {
  Arps_03 <- lapply(na.omit((1:100)[x_months>3]), function(i) StanArpsModel3(x[i]))
  Arps_06 <- lapply(na.omit((1:100)[x_months>6]), function(i) StanArpsModel6(x[i]))
  Arps_09 <- lapply(na.omit((1:100)[x_months>9]), function(i) StanArpsModel9(x[i]))
  Arps_12 <- lapply(na.omit((1:100)[x_months>12]), function(i) StanArpsModel12(x[i]))
  Arps_15 <- lapply(na.omit((1:100)[x_months>15]), function(i) StanArpsModel15(x[i]))
  Arps_18 <- lapply(na.omit((1:100)[x_months>18]), function(i) StanArpsModel18(x[i]))
  Arps_21 <- lapply(na.omit((1:100)[x_months>21]), function(i) StanArpsModel21(x[i]))
  Arps_24 <- lapply(na.omit((1:100)[x_months>24]), function(i) StanArpsModel24(x[i]))
  Arps_36 <- lapply(na.omit((1:100)[x_months>36]), function(i) StanArpsModel36(x[i]))
  Arps_48 <- lapply(na.omit((1:100)[x_months>48]), function(i) StanArpsModel48(x[i]))
  Arps_60 <- lapply(na.omit((1:100)[x_months>60]), function(i) StanArpsModel60(x[i]))
}

#sfStop()


#sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )

#sfExport("temp")
#sfLibrary(lubridate)
#sfLibrary(rstan)
#temp <- sfClusterApplyLB(1:length(x), function(i) StanArpsModel(x[i]))
#b_03 <- sfClusterApplyLB(1:length(Arps_03), function(i) Arps_03[[i]]$b)
#b_06 <- sfClusterApplyLB(1:length(Arps_06), function(i) Arps_06[[i]]$b)
#b_09 <- sfClusterApplyLB(1:length(Arps_09), function(i) Arps_09[[i]]$b)
#b_12 <- sfClusterApplyLB(1:length(Arps_12), function(i) Arps_12[[i]]$b)
#b_15 <- sfClusterApplyLB(1:length(Arps_15), function(i) Arps_15[[i]]$b)
#b_18 <- sfClusterApplyLB(1:length(Arps_18), function(i) Arps_18[[i]]$b)
#b_21 <- sfClusterApplyLB(1:length(Arps_21), function(i) Arps_21[[i]]$b)
#b_24 <- sfClusterApplyLB(1:length(Arps_24), function(i) Arps_24[[i]]$b)

Arps_03 <- readRDS(paste(ResourcePlay, "_Arps_03.rds", sep=""))
b_03 <- lapply(1:length(Arps_03), function(i) Arps_03[[i]]$b)
s2_03 <- lapply(1:length(Arps_03), function(i) Arps_03[[i]]$s2)
rm(Arps_03)
gc()
Arps_06 <- readRDS(paste(ResourcePlay, "_Arps_06.rds", sep=""))
b_06 <- lapply(1:length(Arps_06), function(i) Arps_06[[i]]$b)
s2_06 <- lapply(1:length(Arps_06), function(i) Arps_06[[i]]$s2)
rm(Arps_06)
gc()
Arps_09 <- readRDS(paste(ResourcePlay, "_Arps_09.rds", sep=""))
b_09 <- lapply(1:length(Arps_09), function(i) Arps_09[[i]]$b)
s2_09 <- lapply(1:length(Arps_09), function(i) Arps_09[[i]]$s2)
rm(Arps_09)
gc()
Arps_12 <- readRDS(paste(ResourcePlay, "_Arps_12.rds", sep=""))
b_12 <- lapply(1:length(Arps_12), function(i) Arps_12[[i]]$b)
s2_12 <- lapply(1:length(Arps_12), function(i) Arps_12[[i]]$s2)
rm(Arps_12)
gc()
Arps_15 <- readRDS(paste(ResourcePlay, "_Arps_15.rds", sep=""))
b_15 <- lapply(1:length(Arps_15), function(i) Arps_15[[i]]$b)
s2_15 <- lapply(1:length(Arps_15), function(i) Arps_15[[i]]$s2)
rm(Arps_15)
gc()
Arps_18 <- readRDS(paste(ResourcePlay, "_Arps_18.rds", sep=""))
b_18 <- lapply(1:length(Arps_18), function(i) Arps_18[[i]]$b)
s2_18 <- lapply(1:length(Arps_18), function(i) Arps_18[[i]]$s2)
rm(Arps_18)
gc()
Arps_21 <- readRDS(paste(ResourcePlay, "_Arps_21.rds", sep=""))
b_21 <- lapply(1:length(Arps_21), function(i) Arps_21[[i]]$b)
s2_21 <- lapply(1:length(Arps_21), function(i) Arps_21[[i]]$s2)
rm(Arps_21)
gc()
Arps_24 <- readRDS(paste(ResourcePlay, "_Arps_24.rds", sep=""))
b_24 <- lapply(1:length(Arps_24), function(i) Arps_24[[i]]$b)
s2_24 <- lapply(1:length(Arps_24), function(i) Arps_24[[i]]$s2)
rm(Arps_24)
gc()
Arps_36 <- readRDS(paste(ResourcePlay, "_Arps_36.rds", sep=""))
b_36 <- lapply(1:length(Arps_36), function(i) Arps_36[[i]]$b)
s2_36 <- lapply(1:length(Arps_36), function(i) Arps_36[[i]]$s2)
rm(Arps_36)
gc()
Arps_48 <- readRDS(paste(ResourcePlay, "_Arps_48.rds", sep=""))
b_48 <- lapply(1:length(Arps_48), function(i) Arps_48[[i]]$b)
s2_48 <- lapply(1:length(Arps_48), function(i) Arps_48[[i]]$s2)
rm(Arps_48)
gc()
Arps_60 <- readRDS(paste(ResourcePlay, "_Arps_60.rds", sep=""))
b_60 <- lapply(1:length(Arps_60), function(i) Arps_60[[i]]$b)
s2_60 <- lapply(1:length(Arps_60), function(i) Arps_60[[i]]$s2)
rm(Arps_60)
gc()

#sfStop()

#hist(temp[[7]]$b)
#hist(unlist(temp2),breaks=100)
hist(unlist(b_03),breaks=100)
hist(unlist(b_06),breaks=100)
hist(unlist(b_09),breaks=100)
hist(unlist(b_12),breaks=100)
hist(unlist(b_15),breaks=100)
hist(unlist(b_18),breaks=100)
hist(unlist(b_21),breaks=100)
hist(unlist(b_24),breaks=100)
hist(unlist(b_36),breaks=100)
hist(unlist(b_48),breaks=100)
hist(unlist(b_60),breaks=100)

if(FALSE) {
write.csv(data.frame(mids=hist(unlist(b_03),breaks=100)$mids, density=hist(unlist(b_03),breaks=100)$density),file=paste(ResourcePlay, "_b_03.csv", sep=""), row.names=FALSE)
write.csv(data.frame(mids=hist(unlist(b_06),breaks=100)$mids, density=hist(unlist(b_06),breaks=100)$density),file=paste(ResourcePlay, "_b_06.csv", sep=""), row.names=FALSE)
write.csv(data.frame(mids=hist(unlist(b_09),breaks=100)$mids, density=hist(unlist(b_09),breaks=100)$density),file=paste(ResourcePlay, "_b_09.csv", sep=""), row.names=FALSE)
write.csv(data.frame(mids=hist(unlist(b_12),breaks=100)$mids, density=hist(unlist(b_12),breaks=100)$density),file=paste(ResourcePlay, "_b_12.csv", sep=""), row.names=FALSE)
write.csv(data.frame(mids=hist(unlist(b_15),breaks=100)$mids, density=hist(unlist(b_15),breaks=100)$density),file=paste(ResourcePlay, "_b_15.csv", sep=""), row.names=FALSE)
write.csv(data.frame(mids=hist(unlist(b_18),breaks=100)$mids, density=hist(unlist(b_18),breaks=100)$density),file=paste(ResourcePlay, "_b_18.csv", sep=""), row.names=FALSE)
write.csv(data.frame(mids=hist(unlist(b_21),breaks=100)$mids, density=hist(unlist(b_21),breaks=100)$density),file=paste(ResourcePlay, "_b_21.csv", sep=""), row.names=FALSE)
write.csv(data.frame(mids=hist(unlist(b_24),breaks=100)$mids, density=hist(unlist(b_24),breaks=100)$density),file=paste(ResourcePlay, "_b_24.csv", sep=""), row.names=FALSE)
write.csv(data.frame(mids=hist(unlist(b_36),breaks=100)$mids, density=hist(unlist(b_36),breaks=100)$density),file=paste(ResourcePlay, "_b_36.csv", sep=""), row.names=FALSE)
write.csv(data.frame(mids=hist(unlist(b_48),breaks=100)$mids, density=hist(unlist(b_48),breaks=100)$density),file=paste(ResourcePlay, "_b_48.csv", sep=""), row.names=FALSE)
write.csv(data.frame(mids=hist(unlist(b_60),breaks=100)$mids, density=hist(unlist(b_60),breaks=100)$density),file=paste(ResourcePlay, "_b_60.csv", sep=""), row.names=FALSE)
}


hist(unlist(s2_24),breaks=100)
hist(unlist(s2_36),breaks=100)
hist(unlist(s2_48),breaks=100)
hist(unlist(s2_60),breaks=100)

if(FALSE) {
  hist(unlist(b_3),breaks=10)
  hist(unlist(b_6),breaks=10)
  hist(unlist(b_9),breaks=10)
  hist(unlist(b_12),breaks=10)
  hist(unlist(b_15),breaks=10)
  hist(unlist(b_18),breaks=10)
  hist(unlist(b_21),breaks=10)
  hist(unlist(b_24),breaks=10)
  hist(unlist(b_36),breaks=10)
  hist(unlist(b_48),breaks=10)
  hist(unlist(b_60),breaks=10)
}

#let's build a sample hist for one well's changes over time
i<-7
#plot(log(q)~t,data=x[[i]],main=i)
#plot(q~Q,data=x[[i]],main=i)
sample_well <- rbind(
  data.frame(EUR=Arps_03[[i]]$EUR, b=Arps_3[[i]]$b, Months="3"),
  data.frame(EUR=Arps_06[[i]]$EUR, b=Arps_6[[i]]$b, Months="6"),
  data.frame(EUR=Arps_09[[i]]$EUR, b=Arps_9[[i]]$b, Months="9"),
  data.frame(EUR=Arps_12[[i]]$EUR, b=Arps_12[[i]]$b, Months="12"),
  data.frame(EUR=Arps_15[[i]]$EUR, b=Arps_15[[i]]$b, Months="15"),
  data.frame(EUR=Arps_18[[i]]$EUR, b=Arps_18[[i]]$b, Months="18")
)
ggplot(sample_well, aes(EUR, fill = Months)) + geom_density(alpha = 0.2) + scale_x_continuous(limits = c(0, 10e6)) + ggtitle(names(x[i]))
#ggplot(sample_well, aes(b, fill = Months)) + geom_density(alpha = 0.2) + ggtitle(names(x[i]))


