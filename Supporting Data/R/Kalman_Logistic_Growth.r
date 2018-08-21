#' Return a sequence of Logistic Growth parameters for a list containing population data 
#' at various points in time and a prior estimate of parameters.
#' 
#' NB: This model assumes a growing population: the growth rate is forced to be possitive.
#' 
#' \code{Kalman_Logistic_Growth} requires a list containing at least two items.  The first
#' must be a dataframe containing population data.  At a minimum, the population data must have 
#' POSIX times (t) for each data point, must have population growth rate data (q) and must have
#' observed population data (Q).  At least one of the other items is a dataframe named "Prior"
#' that contains initial estimates of the best fit Logistic Growth parameters.  These are average
#' population growth rate (r), initial population (p0) and ultimate population (p_ult).
#' 
#' The function takes this input and fits a Logistic Growth model to the data.  It uses a Kalman 
#' filter approach.  As the Kalman filter requires a system of linear equations to perform updates,
#' the Logistic Growth model is represented by a series of linear approximations.  These
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
#'   Rate            growth rate
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
#'     r=0.002, 
#'     p0=30, 
#'     p_ult=2000
#'     )
#' )
#' Kalman_Logistic_Growth(data)

Kalman_Logistic_Growth <- function(data, PLOT=FALSE){
  UWI <- names(data[1])
  r <- data$Prior$r
  p0 <- data$Prior$p0
  p_ult <- data$Prior$p_ult
  time <- subset(data[[1]],q>0)$t
  Q <- subset(data[[1]],q>0)$Q
  nObs <- length(time)
  
  if(nObs>0) {
    
    #library(numDeriv)
    
    # Logistic growth function
    logistG <- function(r, p, k, t){
      k * p * exp(r*t) / (k + p * (exp(r*t) - 1))
    }
    
    # Helper functions
    a <- function(x, deltaT){
      c(r=x[1], p_ult=x[2], logistG(r=x[1], p=x[3], k=x[2], deltaT))
    }
    G <- t(c(0, 0, 1))
    
    #Let's initiate our parameters
    pop <- Q
    deltaT <- as.numeric(diff(time))
    deltaT <- c(deltaT[1],deltaT)
    obsVariance <- 100
    Evl_error <- diag(c(0, 0, 0))
    Obs_error <-  obsVariance
    Sigma <-  diag(c(2, 1000, 1000))
    # Prior
    x <- c(r, p_ult, p0)
    Estimate <- data.frame(
      Rate=rep(NA, nObs), 
      UltPopulation=rep(NA,nObs), 
      Population=rep(NA,nObs)
    )
    
    
    for(i in 1:nObs){
      # Observation
      xobs <- c(0, 0, pop[i])
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
      if(!is.na(x[1])) {
        if(x[1]<0) x[1] <- abs(x[1])
      } else {
        if(!is.na(xf[1])) {
          x[1] <- xf[1]
        } else {
          x[1] <- mean(c(data$Prior$r,1))
        }
      }
      if(x[1]>1) x[1] <- mean(c(data$Prior$r,1))
      Sigma <- A %*% Sigma %*% t(A) - K %*% G %*% Sigma %*% t(A) + Evl_error
    }
    
    # Plot output
    if(PLOT){
      op <- par(mfrow=c(2,1))
      plot(y=pop, x=time, t='l', main="Population growth", xlab="Time", ylab="Population")
      lines(y=Estimate$Population, x=time, col="orange", lwd=2)
      legend("bottomright", 
             legend=c("Data", "Estimate"), 
             bty="n",
             col=c("black", "orange"),
             lty=1, lwd=2)
      plot(
        y=Estimate$Rate, 
        x=time, 
        t='l', 
        main="Model Parameters", 
        xlab="Time", 
        ylab="Rate", 
        col="orange", 
        lwd=2
      )
      lines(
        y=Estimate$UltPopulation*max(Estimate$Rate)/max(Estimate$UltPopulation), 
        x=time, 
        col="red", 
        lwd=2)
      legend("topright", 
             legend=c("Growth Rate", "Ult Pop"), 
             bty="n",
             col=c("orange", "red"),
             lty=1, lwd=2)
      par(op)
    }
    
    return(Estimate)
  } else {
    Estimate <- data.frame(
      Rate=rep(NA, 1), 
      UltPopulation=rep(NA,1), 
      Population=rep(NA,1)
    )
    return(Estimate)
  }
}