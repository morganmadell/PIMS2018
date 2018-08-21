#' Identify well production segments that would be apparent on a q vs Q plot
#' 
#' \code{Seg.Well.Anal.Exp}
#' Requires a production dataframe "x" with rownames t,q,Q,qf
#' if the the optional argument PLOT=TRUE, 
#' generates a plot with a bootstrapped estimate of exponential EUR on final segment
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
#' Seg.Well.Anal.Exp(data)
Seg.Well.Anal.Exp <- function(x, PLOT=FALSE, iters=10){
  UWI <- names(x[1])
  x <- x[[1]]
  x <- subset(x,q>0)  
  #x <- x[with(x, order(t)),]
  
  xx <- x
  x <- x[-1,]
  
  #start filtering points
  for(i in 1:2){
    try(z <- gam(q~s(Q), data=x, family=gaussian(link="identity"),method="GCV.Cp",sp=1e-10),TRUE)
    if(exists("z")) {
    z1 <- predict(z, newdata=list(Q = x$Q), se.fit = TRUE)
    upper <- z1$fit + 4*z1$se.fit
    lower <- z1$fit - 2*z1$se.fit
    #plot(xx$Q,xx$q,xlab="Cumulative Production",ylab="Rate",xlim=c(0,1000*ceiling(max(xx$Q/1000))),ylim=c(0,1000*ceiling(max(xx$q/1000))),xaxs="i",yaxs="i")
    #lines(x$Q, z1$fit, type="l")
    #lines(x$Q, upper, lty = 2)
    #lines(x$Q, lower, lty = 2)
    keep <- (upper>x$q) & (x$q>lower)
    x <- x[keep==TRUE,] 
    #points(x$Q,x$q,pch=19)
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
    
  if(length(x$q)>6) {
    breaks <- NA_real_
    try(break.lm.full.mod <- break.lm.full(q~Q,x),TRUE)
    if(exists("break.lm.full.mod")) {
      breaks <- na.omit(break.lm.full.mod$breakpoints)
      if(length(breaks)>1) {
        temp <- lm(q ~ breakfactor(break.lm.full.mod)/Q - 1, data = x)
        results$disc.breaks1 <- x$Q[breaks][1]
        results$disc.breaks2 <- x$Q[breaks][2]
        results$disc.slope1 <- coef(temp)[4]
        results$disc.slope2 <- coef(temp)[5]
        results$disc.slope3 <- coef(temp)[6]
        results$disc.int1 <- coef(temp)[1]
        results$disc.int2 <- coef(temp)[2]
        results$disc.int3 <- coef(temp)[3]
        results$disc.err1 <- pmax(abs(confint(temp)[4,1]),abs(confint(temp)[4,2]))/abs(coef(temp)[4])-1
        results$disc.err2 <- pmax(abs(confint(temp)[5,1]),abs(confint(temp)[5,2]))/abs(coef(temp)[5])-1
        results$disc.err3 <- pmax(abs(confint(temp)[6,1]),abs(confint(temp)[6,2]))/abs(coef(temp)[6])-1
      } else {
        if(length(breaks)>0) {
          temp <- lm(q ~ breakfactor(break.lm.full.mod)/Q - 1, data = x)
          results$disc.breaks1 <- x$Q[breaks][1]
          results$disc.slope1 <- coef(temp)[3]
          results$disc.slope2 <- coef(temp)[4]
          results$disc.int1 <- coef(temp)[1]
          results$disc.int2 <- coef(temp)[2]
          results$disc.err1 <- pmax(abs(confint(temp)[3,1]),abs(confint(temp)[3,2]))/abs(coef(temp)[3])-1
          results$disc.err2 <- pmax(abs(confint(temp)[4,1]),abs(confint(temp)[4,2]))/abs(coef(temp)[4])-1
        } else {
          temp <- lm(q ~ Q, data = x)
          results$disc.slope1 <- coef(temp)[2]
          results$disc.int1 <- coef(temp)[1] 
          results$disc.err1 <- pmax(abs(confint(temp)[2,1]),abs(confint(temp)[2,2]))/abs(coef(temp)[2])-1     
        }
      }
    } else {
      temp <- lm(q ~ Q, data = x)
      results$disc.slope1 <- coef(temp)[2]
      results$disc.int1 <- coef(temp)[1] 
      results$disc.err1 <- pmax(abs(confint(temp)[2,1]),abs(confint(temp)[2,2]))/abs(coef(temp)[2])-1     
    }
    try(segmented.mod <- segmented.lm.break(q~Q,x),TRUE)
    if(exists("segmented.mod")) {
      #breaks <- sapply(segmented.mod$psi[,2], function(y) which.min(abs(x$Q-y)))
      #results$cont.breaks <- segmented.mod$psi[,2]
      #results$bounds.breaks <- segmented.mod$psi[,3]
      if(length(segmented.mod$psi[,2])>1) {
        results$cont.breaks1 <- segmented.mod$psi[1,2]
        results$cont.breaks2 <- segmented.mod$psi[2,2]
        results$bounds.breaks1 <- segmented.mod$psi[1,3]
        results$bounds.breaks2 <- segmented.mod$psi[2,3]
      } else {
        if(length(segmented.mod$psi[,2])>0) {
          results$cont.breaks <- segmented.mod$psi[1,2]
          results$bounds.breaks <- segmented.mod$psi[1,3]
        }
      }
    }
    

    qf <- max(x$qf)
    Qcum <- max(x$Q)
    temp2 <- lm(q ~ Q, x)$coef
    results$EUR <-max(as.numeric((qf-temp2[1])/temp2[2]),Qcum)
    try((Bootstrap <- Boot.Exp.Reserves(tail(x,n=length(x$Q)-tail(breaks,n=1)),iters)),TRUE)
    if(!exists("Bootstrap")) {
      Bootstrap <- Boot.Exp.Reserves(x,iters)
    }
    if(!exists("Bootstrap")) {
      Bootstrap <- rep(NA_real_,iters)
    }
    
    
    if(PLOT){
      plot(xx$Q,xx$q,xlab="Cumulative Production",ylab="Rate",xlim=c(0,1000*ceiling(max(xx$Q/1000))),ylim=c(0,100*ceiling(max(xx$q/100))),xaxs="i",yaxs="i",main=UWI)
      points(x$Q,x$q,pch=19)
      abline(h=max(x$qf),col="gray")
      if(exists("break.lm.full.mod")) {
        if(length(breaks)>0) {
          try(lines(x$Q,fitted(lm(q ~ breakfactor(break.lm.full.mod)/Q - 1, data = x)),col="red"),TRUE)
        } else { 
          try(abline(temp,col="red"),TRUE)
        }
      } else { 
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
        hist(Bootstrap, pch='.', mgp=c(1,0.4,0), xlab='', ylab='', main='',cex.axis=0.5),
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