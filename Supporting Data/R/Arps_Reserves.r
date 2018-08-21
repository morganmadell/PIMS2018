#' Calculate the reserves using a generalized Arps decline
#' 
#' \code{Arps_Reserves}
#'
#' @param x list of data to parse
#' @importFrom aRpsDCA
#' @export
#' @examples
#' data <- list(SampleWell = data.frame(t=seq(as.POSIXct("2000/1/1"), by = "month", length.out = 100), q=5*(100:1), Q=20*(1:100), qf=rep(25,100)))
#' Arps_Reserves(data)
Arps_Reserves <- function(x, ReleaseDate=FALSE){
  AlgorithmReleaseDate <- 2016.0831
  if(ReleaseDate) {
    return(AlgorithmReleaseDate)
  } else {
  UWI <- names(x[1])
  x <- subset(x[[1]],q>0)
  q <- x$q
  Q <- x$Q
  t <- cumsum(c(Q[1],diff(Q))/q)
  ProducingDays <- t[length(t)]
  qf <- max(x$qf)
  qi_Arps <- NA_real_
  b_Arps <- NA_real_
  Di_Arps <- NA_real_
  EUR_Arps <- NA_real_
  Rem_Arps <- NA_real_
  if(length(q)<3) {
    # If only three producing months, don't do much for now
    #If the well hasn't produced for two years, assume it is abandoned and use the cummulative production as the EUR
    if(length(q)>0) {
      if(max(t) < (Sys.time()-60*60*24*365*2)) {
        Cum_Prod <- max(getDataPart(Q))/1000
        EUR_Exp <- Cum_Prod
        EUR_Harm <- Cum_Prod
        Rem_Exp <- 0
        Rem_Harm <- 0
      }
    }
  } else {
    # Check if the well has more than 18 producing months before doing a decline
    if(length(q)>18) {
      ArpsModel <- best.hyperbolic(q, t,
                                   lower=c( # lower bounds
                                     0, # qi > 0
                                     0, # Di > 0
                                     0), # b > 0
                                   upper=c( # upper bounds
                                     max(q) * 5, # qi < qmax * 5
                                     10, # = 0.99995 / [time] effective
                                     2) # b <= 2.0
      )
      if(arps.eur(ArpsModel$decline, qf)>max(getDataPart(Q))) {
        qi_Arps <- ArpsModel$decline$qi
        b_Arps <- ArpsModel$decline$b
        Di_Arps <- ArpsModel$decline$Di
        EUR_Arps <- arps.eur(ArpsModel$decline, qf)
        Rem_Arps <- EUR_Arps - max(getDataPart(Q))
      } else {
        ArpsModel <- best.hyperbolic.from.Np(Q, t,
                                             lower=c( # lower bounds
                                               0, # qi > 0
                                               0, # Di > 0
                                               0), # b > 0
                                             upper=c( # upper bounds
                                               max(c(Q[1], diff(Q)) / diff(c(0, t))) * 5, # qi < max(rate) * 5
                                               10, # = 0.99995 / [time] effective
                                               2) # b <= 2.0
        )
        if(arps.eur(ArpsModel$decline, qf)>max(getDataPart(Q))) {
          qi_Arps <- ArpsModel$decline$qi
          b_Arps <- ArpsModel$decline$b
          Di_Arps <- ArpsModel$decline$Di
          EUR_Arps <- arps.eur(ArpsModel$decline, qf)
          Rem_Arps <- EUR_Arps - max(getDataPart(Q))
        } else {
          EUR_Arps <- max(getDataPart(Q))
          Rem_Arps <- 0
        }
      }
    } else {
      #If less than 18 months, don't do anything (for now)
    }
  }  # end of the if statement to check for more than 3 data points
  return(c(qi_Arps, b_Arps, Di_Arps*100, EUR_Arps/1000, Rem_Arps/1000, qf, AlgorithmReleaseDate, ProducingDays))
  }
}