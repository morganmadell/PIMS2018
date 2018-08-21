#' Identify well production segments that would be apparent on a 1/q vs sqrt(t) plot
#' 
#' \code{Seg.Well.Anal.Watten}
#' Requires a production dataframe "x" with rownames t,q,Q,qf
#' if the the optional argument PLOT=TRUE, 
#' generates a plot with a bootstrapped estimate of linear flow EUR on final segment
#'
#' Returns a three column dataframe:
#'   disc.breaks     breaks from fitting discontinuous linear trends, units are the same as Q
#'   cont.breaks     breaks from fitting continuous linear trends, units are the same as Q
#'   bounds.breaks   estimates of the standard error for each cont.breaks
#'
#' @param x data frame to parse
#' @param PLOT plot results to current device (default PLOT=FALSE)
#' @param iters number of iteration to use for bootstrap estimates
#' @importFrom mgcv gam
#' @export
#' @examples
#' data <- list(SampleWell = data.frame(t=seq(as.POSIXct("2000/1/1"), by = "month", length.out = 100), q=5*(100:1), Q=20*(1:100), qf=rep(25,100)))
#' Seg.Well.Anal.Watten(data)
Seg.Well.Anal.Watten <- function(x, PLOT=FALSE, iters=10){
  UWI <- names(x[1])
  x <- x[[1]]
  x <- subset(x,q>0)
  #x <- x[with(x, order(t)),]
  t_temp <- sqrt(as.numeric(difftime(x$t,x$t[1],units="days"))[-1])
  x$t <- as.numeric(x$t)
  x$t[-1] <- t_temp
  x <- x[-1,]  
  
  xx <- x  
  
  #start filtering points
  for(i in 1:2){
    try(z <- gam(1/q~s(t), data=x, family=gaussian(link="identity"),method="GCV.Cp",sp=1e-1),TRUE)
    if(exists("z")) {
    z1 <- predict(z, newdata=list(t = x$t), se.fit = TRUE)
    upper <- z1$fit + 1*z1$se.fit
    lower <- z1$fit - 4*z1$se.fit
    #plot(xx$t,(1/xx$q),xlab="Cumulative Production",ylab="Rate",xlim=c(0,1000*ceiling(max(xx$t/1000))),ylim=c(0,1000*ceiling(max((1/xx$q)/1000))),xaxs="i",yaxs="i")
    #lines(x$t, z1$fit, type="l")
    #lines(x$t, upper, lty = 2)
    #lines(x$t, lower, lty = 2)
    keep <- (upper>(1/x$q)) & ((1/x$q)>lower)
    x <- x[keep==TRUE,] 
    #points(x$t,(1/x$q),pch=19)
    }
  }
  
  results <- data.frame(
    disc.breaks1=NA_real_,
    disc.slope1=NA_real_,
    disc.slope2=NA_real_,
    disc.int1=NA_real_,
    disc.int2=NA_real_,
    disc.err1=NA_real_,
    disc.err2=NA_real_,
    cont.breaks=NA_real_, 
    bounds.breaks=NA_real_,
    EUR=NA_real_
  )
  
  #sqrt(as.numeric(difftime(t,t[1],units="days"))[-1])
  
  if(length(x$q)>6) {
    breaks <- NA_real_
    try(break.lm.full.mod <- break.lm.full(1/q~t,x),TRUE)
    if(exists("break.lm.full.mod")) {
      breaks <- na.omit(break.lm.full.mod$breakpoints)
      if(length(breaks)>1) {
        temp <- lm(1/q ~ breakfactor(break.lm.full.mod)/t - 1, data = x)
        results$disc.breaks1 <- x$t[breaks][1]
        results$disc.breaks2 <- x$t[breaks][2]
        results$disc.slope1 <- coef(temp)[4]
        results$disc.slope2 <- coef(temp)[5]
        results$disc.slope3 <- coef(temp)[6]
        results$disc.int1 <- coef(temp)[1]
        results$disc.int2 <- coef(temp)[2]
        results$disc.int3 <- coef(temp)[3]
        results$disc.err1 <- pmax(abs(confint(temp)[4,1]),abs(confint(temp)[4,2]))/abs(coef(temp)[4])-1
        results$disc.err2 <- pmax(abs(confint(temp)[5,1]),abs(confint(temp)[5,2]))/abs(coef(temp)[5])-1
        results$disc.err3 <- pmax(abs(confint(temp)[6,1]),abs(confint(temp)[6,2]))/abs(coef(temp)[6])-1
      } 
      else {
        if(length(breaks)>0) {
          temp <- lm(1/q ~ breakfactor(break.lm.full.mod)/t - 1, data = x)
          results$disc.breaks1 <- x$t[breaks][1]
          results$disc.slope1 <- coef(temp)[3]
          results$disc.slope2 <- coef(temp)[4]
          results$disc.int1 <- coef(temp)[1]
          results$disc.int2 <- coef(temp)[2]
          results$disc.err1 <- pmax(abs(confint(temp)[3,1]),abs(confint(temp)[3,2]))/abs(coef(temp)[3])-1
          results$disc.err2 <- pmax(abs(confint(temp)[4,1]),abs(confint(temp)[4,2]))/abs(coef(temp)[4])-1
        }
        else {
          temp <- lm(1/q ~ t, data = x)
          results$disc.slope1 <- coef(temp)[2]
          results$disc.int1 <- coef(temp)[1] 
          results$disc.err1 <- pmax(abs(confint(temp)[2,1]),abs(confint(temp)[2,2]))/abs(coef(temp)[2])-1     
        }
      }
    }
    else {
      temp <- lm(1/q ~ t, data = x)
      results$disc.slope1 <- coef(temp)[2]
      results$disc.int1 <- coef(temp)[1] 
      results$disc.err1 <- pmax(abs(confint(temp)[2,1]),abs(confint(temp)[2,2]))/abs(coef(temp)[2])-1     
    }
    try(segmented.mod <- segmented.lm.break(1/q~t,x),TRUE)
    if(exists("segmented.mod")) {
      #breaks <- sapply(segmented.mod$psi[,2], function(y) which.min(abs(x$Q-y)))
      #results$cont.breaks <- segmented.mod$psi[,2]
      #results$bounds.breaks <- segmented.mod$psi[,3]
      if(length(segmented.mod$psi[,2])>1) {
        results$cont.breaks1 <- 2^(segmented.mod$psi[1,2])
        results$cont.breaks2 <- 2^(segmented.mod$psi[2,2])
        results$bounds.breaks1 <- 2^(segmented.mod$psi[1,3])
        results$bounds.breaks2 <- 2^(segmented.mod$psi[2,3])
      }
      else {
        if(length(segmented.mod$psi[,2])>0) {
          results$cont.breaks1 <- 2^(segmented.mod$psi[1,2])
          results$bounds.breaks1 <- 2^(segmented.mod$psi[1,3])
        }
      }
    }
    
    
    qf <- max(x$qf)
    Qcum <- max(x$Q)
    temp2 <- lm(1/q ~ t, x)$coef
    EUR <- as.numeric(
      x$Q[1] + 
        (2/(temp2[2]^2))*(
          (1/qf-1/x$q[1]) + 
            temp2[1]*(log(qf)-log(x$q[1]))
        )
    )
    results$EUR <- max(EUR,Qcum)
    try((Bootstrap <- Boot.Watten.Reserves(tail(x,n=length(x$Q)-tail(breaks,n=1)),iters)),TRUE)
    if(!exists("Bootstrap")) {
      Bootstrap <- Boot.Watten.Reserves(x,iters)
    }
    if(!exists("Bootstrap")) {
      Bootstrap <- rep(NA_real_,iters)
    }
    
    
    if(PLOT){
      plot(xx$t,1/xx$q,ylab="1/Rate",xlab=expression(paste(sqrt(t), "  ",(sqrt(days)))),xaxs="i",yaxs="i",ylim=c(0,max(1/x$q)),main=UWI)
      points(x$t,1/x$q,pch=19)
      abline(h=1/max(x$qf),col="gray")
      if(exists("break.lm.full.mod")) {
        if(length(breaks)>0) {
          try(lines(x$t,fitted(lm(1/q ~ breakfactor(break.lm.full.mod)/t - 1, data = x)),col="red"),TRUE)
        }
        else { 
          try(abline(temp,col="red"),TRUE)
        }
      }
      else { 
        try(abline(temp,col="red"),TRUE)
      }
      if(exists("segmented.mod")) plot(segmented.mod, add=T, col="blue")
      if(!(length(breaks)>0)) breaks <- 0
      text(
        x=par("usr")[2]*0.50,
        y=par("usr")[4]*0.95,
        pos=1,
        cex=1.0,
        paste("EUR = ", formatC(results$EUR, digits=0, big.mark = ",", format = "f"), sep="")
      )
      try(subplot(
        hist(Bootstrap, pch='.', mgp=c(1,0.4,0), xlab='', ylab='',main="",cex.axis=0.5),
        x=grconvertX(c(0.75,1.00), from='npc'),
        y=grconvertY(c(0.75,1.00), from='npc'),
        #type='fig',
        pars=list( mar=c(1.5,1.5,0,0)+0.1)
      ),TRUE)
    }}
  
  if(!exists("Bootstrap")) {
    Bootstrap <- rep(NA_real_,iters)
  }
  
  return(c(as.vector(results),Bootstrap))
}