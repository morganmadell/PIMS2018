#' Return a sequence of Power Law parameters for a list containing population data 
#' at various points in time and a prior estimate of parameters.
#' 
#' NB: This model assumes a growing population: the growth rate is forced to be possitive.
#' 
#' \code{Kalman_Power_Law} requires a list containing at least two items.  The first
#' must be a dataframe containing population data.  At a minimum, the population data must have 
#' POSIX times (t) for each data point, must have population growth rate data (q) and must have
#' observed population data (Q).  At least one of the other items is a dataframe named "Prior"
#' that contains initial estimates of the best fit Logistic Growth parameters.  These are initial
#' production rate (q0), intitial decline rate (D0), ultimate decline rate (Dinf) and decline
#' exponent (n)
#' 
#' The function takes this input and fits a Power Law model to the data.  It uses a Kalman 
#' filter approach.  As the Kalman filter requires a system of linear equations to perform updates,
#' the Power Law Decline model is represented by a series of linear approximations.  These
#' approximations are provided by taking the Jacobian of the Logistic Growth model, which, in
#' turn, is calculated at each time step using a numerical derivate provided by the "jacobian" 
#' function of the numDeriv package.
#' 
#' If the the optional argument PLOT=TRUE,then returns a plot with the production data overlaid
#' with the model fit and a continuous trace for all model parameters
#' 
#' Much of the code was taken directly from 
#' http://www.magesblog.com/2015/01/extended-kalman-filter-example-in-r.html
#'
#' Returns dataframe containing estimates of the following for each time step of the 
#' inputted prodution data:
#'   Rate            production rate
#'   UltPopulation   ultimate population (aka carrying capacity)
#'   Population      population
#'
#' @param data list of data to parse
#' @param PLOT plot results to current device (default PLOT=FALSE)
#' @importFrom  numDeriv jacobian
#' @export
#' @examples
#' data <- list(
#'   SampleWell = data.frame(
#'     t=seq(as.POSIXct("2000/1/1"), by = "month", length.out = 100), 
#'     q=5*(100:1), 
#'     Q=20*(1:100)
#'     ),
#'   Prior = data.frame(
#'     q0=500, 
#'     D0=0.2, 
#'     Dinf=0.08, 
#'     n=0.3
#'     )
#' )
#' Kalman_Power_Law(data)


Kalman_Power_Law <- function(data, PLOT=FALSE){
  UWI <- names(data[1])
  q0 <- data$Prior$q0
  D0 <- data$Prior$D0
  Dinf <- data$Prior$Dinf
  n <- data$Prior$n
  time <- subset(data[[1]],q>0)$t
  rate <- subset(data[[1]],q>0)$q
  nObs <- length(time)
  
  #library(numDeriv)
  
  # Logistic growth function
  Power_Law <- function(q0, D0, Dinf, n, t){
    q0 * exp(-Dinf * t - D0 * t^n)
  }
  
  
  # Helper functions
  a <- function(x, deltaT){
    c(Power_Law(q0=x[1], D0=x[2], Dinf=x[3], n=x[4], deltaT), q0=x[1], D0=x[2], Dinf=x[3], n=x[4])
  }
  G <- t(c(0, 0, 0, 0, 1))
  
  #Let's initiate our parameters
  #deltaT <- as.numeric(diff(time))
  #deltaT <- c(deltaT[1],deltaT)
  deltaT <- as.numeric(time-time[1])/(365*24*60*60)
  obsVariance <- 10
  Evl_error <- diag(c(1, 0, 0, 0, 1))
  Obs_error <-  obsVariance
  Sigma <-  diag(c(10, 2, 2, 2, 10))
  # Prior
  x <- c(q0, D0, Dinf, n, q0)
  Estimate <- data.frame(
    InitialRate=rep(NA, nObs), 
    InitialDecline=rep(NA,nObs), 
    UltimateDecline=rep(NA,nObs), 
    DeclineExpon=rep(NA,nObs), 
    Rate=rep(NA, nObs)
    )
  
  
  for(i in 1:nObs){
    # Observation
    xobs <- c(0, 0, 0, 0, rate[i])
    y <- G %*% xobs
    
    # Filter  
    SigTermInv <- solve(G %*% Sigma %*% t(G) + Obs_error)
    xf <- x + Sigma %*% t(G) %*%  SigTermInv %*% (y - G %*% x)
    Sigma <- Sigma - Sigma %*% t(G) %*% SigTermInv %*% G %*% Sigma 
    
    A <- jacobian(a, x=x, deltaT=deltaT[i])   
    K <- A %*% Sigma %*% t(G) %*% solve(G %*% Sigma %*% t(G) + Obs_error)
    Estimate[i,] <- x
    
    # Predict
    x <- a(x=xf, deltaT=deltaT[i]) + K %*% (y - G %*% xf)
    if(x[1]<0) x[1] <- abs(x[1])
    if(x[2]<0) x[2] <- abs(x[2])
    if(x[3]<0) x[3] <- abs(x[3])
    if(x[4]<0) x[4] <- abs(x[4])
    #if(x[5]<0) x[5] <- abs(x[5])
    if(x[2]>2) x[2] <- mean(c(data$Prior$D0,1))
    if(x[3]>2) x[3] <- mean(c(data$Prior$Dinf,1))
    Sigma <- A %*% Sigma %*% t(A) - K %*% G %*% Sigma %*% t(A) + Evl_error
  }
  
  # Plot output
  if(PLOT){
    op <- par(mfrow=c(2,1))
    plot(y=rate, x=time, t='l', main="Production Rate", xlab="Time", ylab="Rate")
    lines(y=Estimate$Rate, x=time, col="orange", lwd=2)
    legend("bottomright", 
           legend=c("Data", "Estimate"), 
           bty="n",
           col=c("black", "orange"),
           lty=1, lwd=2)
    plot(
      y=Estimate$InitialDecline, 
      x=time, 
      t='l', 
      main="Model Parameters", 
      xlab="Time", 
      ylab="Initial Decline", 
      col="orange", 
      lwd=2
      )
    lines(
      y=Estimate$UltimateDecline*max(Estimate$InitialDecline)/max(Estimate$UltimateDecline), 
      x=time, 
      col="red", 
      lwd=2)
    legend("topright", 
           legend=c("Initial Decline", "Ultimate Decline"), 
           bty="n",
           col=c("orange", "red"),
           lty=1, lwd=2)
    par(op)
  }
  
  return(Estimate)
}