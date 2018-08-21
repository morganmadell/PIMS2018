#' @import rstan

detachPackage("snowfall")
detachPackage("doSNOW")
detachPackage("snow")
library(rstan)
library(parallel)
options(mc.cores = parallel::detectCores())
#options(mc.cores = 1)
#control=list(adapt_delta=0.9)

CompiledLgmModel <- stan_model(file=paste(workdir, "R/", "LgmModel.stan", sep=""), auto_write=TRUE)
#CompiledLgmModel <- stan_model(file=paste(workdir, "R/", "LgmModel.stan", sep=""),auto_write=FALSE)

StanLgmModel <- function(x){
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
  
  LgmResults <- sampling(
    CompiledLgmModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","n","K","EUR","s2","s3")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(LgmResults)))
}

StanLgmModel03 <- function(x){
  UWI <- names(x[1])
  x3 <- head(subset(x[[1]],q>0),3)
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
  
  LgmResults <- sampling(
    CompiledLgmModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","n","K","EUR","s2","s3")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(LgmResults)))
}

StanLgmModel06 <- function(x){
  UWI <- names(x[1])
  x3 <- head(subset(x[[1]],q>0),6)
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
  
  LgmResults <- sampling(
    CompiledLgmModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","n","K","EUR","s2","s3")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(LgmResults)))
}

StanLgmModel09 <- function(x){
  UWI <- names(x[1])
  x3 <- head(subset(x[[1]],q>0),9)
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
  
  LgmResults <- sampling(
    CompiledLgmModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","n","K","EUR","s2","s3")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(LgmResults)))
}

StanLgmModel12 <- function(x){
  UWI <- names(x[1])
  x3 <- head(subset(x[[1]],q>0),12)
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
  
  LgmResults <- sampling(
    CompiledLgmModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","n","K","EUR","s2","s3")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(LgmResults)))
}

StanLgmModel15 <- function(x){
  UWI <- names(x[1])
  x3 <- head(subset(x[[1]],q>0),15)
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
  
  LgmResults <- sampling(
    CompiledLgmModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","n","K","EUR","s2","s3")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(LgmResults)))
}

StanLgmModel18 <- function(x){
  UWI <- names(x[1])
  x3 <- head(subset(x[[1]],q>0),18)
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
  
  LgmResults <- sampling(
    CompiledLgmModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","n","K","EUR","s2","s3")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(LgmResults)))
}

StanLgmModel21 <- function(x){
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
  
  LgmResults <- sampling(
    CompiledLgmModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","n","K","EUR","s2","s3")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(LgmResults)))
}

StanLgmModel24 <- function(x){
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
  
  LgmResults <- sampling(
    CompiledLgmModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","n","K","EUR","s2")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(LgmResults)))
}

StanLgmModel36 <- function(x){
  UWI <- names(x[1])
  x2 <- subset(x[[1]],q>0)
  x3 <- tail(head(subset(x[[1]],q>0),54),48)
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
  
  LgmResults <- sampling(
    CompiledLgmModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","n","K","EUR","s2","s3")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(LgmResults)))
}

StanLgmModel48 <- function(x){
  UWI <- names(x[1])
  x2 <- subset(x[[1]],q>0)
  x3 <- tail(head(subset(x[[1]],q>0),66),60)
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
  
  LgmResults <- sampling(
    CompiledLgmModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","n","K","EUR","s2","s3")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(LgmResults)))
}

StanLgmModel60 <- function(x){
  UWI <- names(x[1])
  x2 <- subset(x[[1]],q>0)
  x3 <- tail(head(subset(x[[1]],q>0),78),72)
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
  
  LgmResults <- sampling(
    CompiledLgmModel, 
    data=StanProduction,
    iter = iters,
    pars=c("a","n","K","EUR","s2","s3")
  )
  
  closeAllConnections()
  return(as.data.frame(extract(LgmResults)))
}




#LgmResults <- stan(
#  model_code = LgmModel,
#  data = StanProduction,
#  iter = 2000
#)
#hist(extract(LgmResults)$EUR,breaks=100)
#hist(extract(LgmResults)$Di,breaks=100)
#hist(extract(LgmResults)$qi,breaks=100)

i<-5
#plot(log(q)~t,data=x[[i]],main=i)
plot(q~Q,data=x[[i]],main=i)
#temp <- StanLgmModel03(x[i])
#temp <- StanLgmModel06(x[i])
#temp <- StanLgmModel09(x[i])
#temp <- StanLgmModel12(x[i])
#temp <- StanLgmModel15(x[i])
#temp <- StanLgmModel18(x[i])
#temp <- StanLgmModel21(x[i])
temp <- StanLgmModel24(x[i])
#temp <- StanLgmModel36(x[i])
#temp <- StanLgmModel48(x[i])
#temp <- StanLgmModel60(x[i])
#hist(temp$s2)
#hist(temp$a)
#hist(temp$n)
hist(temp$K)


x_months <- unlist(lapply(1:length(x), function(i) length(subset(x[[i]],q>0)$t)))
Lgm_03 <- lapply((1:length(x))[x_months>3], function(i) StanLgmModel03(x[i]))
saveRDS(Lgm_03, paste(ResourcePlay, "_Lgm_03.rds", sep=""))
rm(Lgm_03)
gc()
Lgm_06 <- lapply((1:length(x))[x_months>6], function(i) StanLgmModel06(x[i]))
saveRDS(Lgm_06, paste(ResourcePlay, "_Lgm_06.rds", sep=""))
rm(Lgm_06)
gc()
Lgm_09 <- lapply((1:length(x))[x_months>9], function(i) StanLgmModel09(x[i]))
saveRDS(Lgm_09, paste(ResourcePlay, "_Lgm_09.rds", sep=""))
rm(Lgm_09)
gc()
Lgm_12 <- lapply((1:length(x))[x_months>12], function(i) StanLgmModel12(x[i]))
saveRDS(Lgm_12, paste(ResourcePlay, "_Lgm_12.rds", sep=""))
rm(Lgm_12)
gc()
Lgm_15 <- lapply((1:length(x))[x_months>15], function(i) StanLgmModel15(x[i]))
saveRDS(Lgm_15, paste(ResourcePlay, "_Lgm_15.rds", sep=""))
rm(Lgm_15)
gc()
Lgm_18 <- lapply((1:length(x))[x_months>18], function(i) StanLgmModel18(x[i]))
saveRDS(Lgm_18, paste(ResourcePlay, "_Lgm_18.rds", sep=""))
rm(Lgm_18)
gc()
Lgm_21 <- lapply((1:length(x))[x_months>21], function(i) StanLgmModel21(x[i]))
saveRDS(Lgm_21, paste(ResourcePlay, "_Lgm_21.rds", sep=""))
rm(Lgm_21)
gc()
Lgm_24 <- lapply((1:length(x))[x_months>24], function(i) StanLgmModel24(x[i]))
saveRDS(Lgm_24, paste(ResourcePlay, "_Lgm_24.rds", sep=""))
rm(Lgm_24)
gc()
Lgm_36 <- lapply((1:length(x))[x_months>36], function(i) StanLgmModel36(x[i]))
saveRDS(Lgm_36, paste(ResourcePlay, "_Lgm_36.rds", sep=""))
rm(Lgm_36)
gc()
Lgm_48 <- lapply((1:length(x))[x_months>48], function(i) StanLgmModel48(x[i]))
saveRDS(Lgm_48, paste(ResourcePlay, "_Lgm_48.rds", sep=""))
rm(Lgm_48)
gc()
Lgm_60 <- lapply((1:length(x))[x_months>60], function(i) StanLgmModel60(x[i]))
saveRDS(Lgm_60, paste(ResourcePlay, "_Lgm_60.rds", sep=""))
rm(Lgm_60)
gc()

if(FALSE) {
  Lgm_03 <- mclapply((1:length(x))[x_months>3], function(i) StanLgmModel03(x[i]))
  Lgm_06 <- mclapply((1:length(x))[x_months>6], function(i) StanLgmModel06(x[i]))
  Lgm_09 <- mclapply((1:length(x))[x_months>9], function(i) StanLgmModel09(x[i]))
  Lgm_12 <- mclapply((1:length(x))[x_months>12], function(i) StanLgmModel12(x[i]))
  Lgm_15 <- mclapply((1:length(x))[x_months>15], function(i) StanLgmModel15(x[i]))
  Lgm_18 <- mclapply((1:length(x))[x_months>18], function(i) StanLgmModel18(x[i]))
  Lgm_21 <- mclapply((1:length(x))[x_months>21], function(i) StanLgmModel21(x[i]))
  Lgm_24 <- mclapply((1:length(x))[x_months>24], function(i) StanLgmModel24(x[i]))
  Lgm_36 <- mclapply((1:length(x))[x_months>36], function(i) StanLgmModel36(x[i]))
  Lgm_48 <- mclapply((1:length(x))[x_months>48], function(i) StanLgmModel48(x[i]))
  Lgm_60 <- mclapply((1:length(x))[x_months>60], function(i) StanLgmModel60(x[i]))
}

if(FALSE) {
  Lgm_03 <- lapply(na.omit((1:100)[x_months>3]), function(i) StanLgmModel03(x[i]))
  Lgm_06 <- lapply(na.omit((1:100)[x_months>6]), function(i) StanLgmModel06(x[i]))
  Lgm_09 <- lapply(na.omit((1:100)[x_months>9]), function(i) StanLgmModel09(x[i]))
  Lgm_12 <- lapply(na.omit((1:100)[x_months>12]), function(i) StanLgmModel12(x[i]))
  Lgm_15 <- lapply(na.omit((1:100)[x_months>15]), function(i) StanLgmModel15(x[i]))
  Lgm_18 <- lapply(na.omit((1:100)[x_months>18]), function(i) StanLgmModel18(x[i]))
  Lgm_21 <- lapply(na.omit((1:100)[x_months>21]), function(i) StanLgmModel21(x[i]))
  Lgm_24 <- lapply(na.omit((1:100)[x_months>24]), function(i) StanLgmModel24(x[i]))
  Lgm_36 <- lapply(na.omit((1:100)[x_months>36]), function(i) StanLgmModel36(x[i]))
  Lgm_48 <- lapply(na.omit((1:100)[x_months>48]), function(i) StanLgmModel48(x[i]))
  Lgm_60 <- lapply(na.omit((1:100)[x_months>60]), function(i) StanLgmModel60(x[i]))
}

Lgm_03 <- readRDS(paste(ResourcePlay, "_Lgm_03.rds", sep=""))
n_03 <- lapply(1:length(Lgm_03), function(i) Lgm_03[[i]]$n)
rm(Lgm_03)
gc()
Lgm_06 <- readRDS(paste(ResourcePlay, "_Lgm_06.rds", sep=""))
n_06 <- lapply(1:length(Lgm_06), function(i) Lgm_06[[i]]$n)
rm(Lgm_06)
gc()
Lgm_09 <- readRDS(paste(ResourcePlay, "_Lgm_09.rds", sep=""))
n_09 <- lapply(1:length(Lgm_09), function(i) Lgm_09[[i]]$n)
rm(Lgm_09)
gc()
Lgm_12 <- readRDS(paste(ResourcePlay, "_Lgm_12.rds", sep=""))
n_12 <- lapply(1:length(Lgm_12), function(i) Lgm_12[[i]]$n)
rm(Lgm_12)
gc()
Lgm_15 <- readRDS(paste(ResourcePlay, "_Lgm_15.rds", sep=""))
n_15 <- lapply(1:length(Lgm_15), function(i) Lgm_15[[i]]$n)
rm(Lgm_15)
gc()
Lgm_18 <- readRDS(paste(ResourcePlay, "_Lgm_18.rds", sep=""))
n_18 <- lapply(1:length(Lgm_18), function(i) Lgm_18[[i]]$n)
rm(Lgm_18)
gc()
Lgm_21 <- readRDS(paste(ResourcePlay, "_Lgm_21.rds", sep=""))
n_21 <- lapply(1:length(Lgm_21), function(i) Lgm_21[[i]]$n)
rm(Lgm_21)
gc()
Lgm_24 <- readRDS(paste(ResourcePlay, "_Lgm_24.rds", sep=""))
n_24 <- lapply(1:length(Lgm_24), function(i) Lgm_24[[i]]$n)
rm(Lgm_24)
gc()
Lgm_36 <- readRDS(paste(ResourcePlay, "_Lgm_36.rds", sep=""))
n_36 <- lapply(1:length(Lgm_36), function(i) Lgm_36[[i]]$n)
rm(Lgm_36)
gc()
Lgm_48 <- readRDS(paste(ResourcePlay, "_Lgm_48.rds", sep=""))
n_48 <- lapply(1:length(Lgm_48), function(i) Lgm_48[[i]]$n)
rm(Lgm_48)
gc()
Lgm_60 <- readRDS(paste(ResourcePlay, "_Lgm_60.rds", sep=""))
n_60 <- lapply(1:length(Lgm_60), function(i) Lgm_60[[i]]$n)
rm(Lgm_60)
gc()


#hist(temp[[7]]$b)
#hist(unlist(temp2),breaks=100)
hist(unlist(n_03),breaks=100)
hist(unlist(n_06),breaks=100)
hist(unlist(n_09),breaks=100)
hist(unlist(n_12),breaks=100)
hist(unlist(n_15),breaks=100)
hist(unlist(n_18),breaks=100)
hist(unlist(n_21),breaks=100)
hist(unlist(n_24),breaks=100)
hist(unlist(n_36),breaks=100)
hist(unlist(n_48),breaks=100)
hist(unlist(n_60),breaks=100)

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
i<-8
#plot(log(q)~t,data=x[[i]],main=i)
plot(q~Q,data=x[[i]],main=i)
sample_well <- rbind(
  data.frame(EUR=Lgm_03[[i]]$EUR, b=Lgm_03[[i]]$b, Months="3"),
  data.frame(EUR=Lgm_06[[i]]$EUR, b=Lgm_06[[i]]$b, Months="6"),
  data.frame(EUR=Lgm_09[[i]]$EUR, b=Lgm_09[[i]]$b, Months="9"),
  data.frame(EUR=Lgm_12[[i]]$EUR, b=Lgm_12[[i]]$b, Months="12"),
  data.frame(EUR=Lgm_15[[i]]$EUR, b=Lgm_15[[i]]$b, Months="15"),
  data.frame(EUR=Lgm_18[[i]]$EUR, b=Lgm_18[[i]]$b, Months="18"),
  data.frame(EUR=Lgm_24[[i]]$EUR, b=Lgm_24[[i]]$b, Months="24"),
  data.frame(EUR=Lgm_36[[i]]$EUR, b=Lgm_36[[i]]$b, Months="36"),
  data.frame(EUR=Lgm_48[[i]]$EUR, b=Lgm_48[[i]]$b, Months="48"),
  data.frame(EUR=Lgm_60[[i]]$EUR, b=Lgm_60[[i]]$b, Months="60")
)
ggplot(sample_well, aes(EUR, fill = Months)) + geom_density(alpha = 0.2) + scale_x_continuous(limits = c(0, 1e6)) + ggtitle(names(x[i]))
#ggplot(sample_well, aes(b, fill = Months)) + geom_density(alpha = 0.2) + ggtitle(names(x[i]))


