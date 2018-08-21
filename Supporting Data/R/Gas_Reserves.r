#' Calculate the gas reserves using several different exponential and harmonic declines.
#' 
#' \code{Gas_Reserves}
#'
#' @param x list of data to parse
#' @importFrom MASS lqs
#' @importFrom LambertW W
#' @export
#' @examples
#' data <- list(SampleWell = data.frame(t=seq(as.POSIXct("2000/1/1"), by = "month", length.out = 100), q=5*(100:1), Q=20*(1:100), qf=rep(25,100)))
#' Gas_Reserves(data)
Gas_Reserves <- function(x, ReleaseDate=FALSE){
  AlgorithmReleaseDate <- 2016.0831
  if(ReleaseDate) {
    return(AlgorithmReleaseDate)
  } else {
    UWI <- names(x[1])
    x <- subset(x[[1]],q>0)
    t <- x$t
    q <- x$q
    Q <- x$Q
    qf <- max(x$qf)
    qi_exp <- NA_real_
    qi_harm <- NA_real_
    EUR_Exp <- NA_real_
    EUR_Harm <- NA_real_
    Rem_Exp <- NA_real_
    Rem_Harm <- NA_real_
    D_exp <- NA_real_
    D_harm <- NA_real_
    D_j <- rep(NA_real_, 30)
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
        Cum_Prod <- max(getDataPart(Q))/1000
        # Calculate the EUR using an exponential ARP's decline
        decline_exp <- lqs(Q/1000, q,method="lms")
        decline_harm <- lqs(Q/1000, log(q),method="lms")
        if(!(decline_harm$coefficients[1]>0 & decline_harm$coefficients[2]<0)) {
          decline_nsl <- nls(log(q)~a+b*Q/1000,algorithm="port", start=c(a=5,b=-10),lower=c(a=0,b=-Inf),upper=c(a=Inf,b=0),control=nls.control(warnOnly=TRUE))
          decline_harm$coefficients[1]<- coef(decline_nsl)[1]
          decline_harm$coefficients[2]<- coef(decline_nsl)[2]
        }
        D_exp <- -decline_exp$coefficients[2]*365/1000
        Di_harm <- -decline_harm$coefficients[2]*exp(decline_harm$coefficients[1])*365/1000
        if(is.infinite(Di_harm)) Di_harm <- D_exp
        if(is.nan(Di_harm)) Di_harm <- D_exp
        D_harm <- Di_harm*exp(Cum_Prod*decline_harm$coefficients[2])
        if(D_exp<0.04) {
          # If the decline is less than 4%, force a 4% decline using the last two years of production
          D_exp <- 0.04
          EUR_Exp <- max(((qf-(decline_exp$coefficients[2]*Cum_Prod+decline_exp$coefficients[1] - (-D_exp*1000/365)*Cum_Prod))/(-D_exp*1000/365)),Cum_Prod)
          Rem_Exp <- EUR_Exp - Cum_Prod
        } else {
          # If the decline is steeper than 4%, use the decline to calculate EUR
          EUR_Exp <- max(((qf-decline_exp$coefficients[1])/decline_exp$coefficients[2]),Cum_Prod)
          Rem_Exp <- EUR_Exp - Cum_Prod
        }
        if(FALSE) { #Di_harm<D_exp
          decline_harm$coefficients[1] <- W(-(-D_exp/0.365)*(Cum_Prod/2)*exp(-(decline_harm$coefficients[2]*(Cum_Prod/2)+decline_harm$coefficients[1]))) + (decline_harm$coefficients[2]*(Cum_Prod/2)+decline_harm$coefficients[1])
          decline_harm$coefficients[2] <- -D_exp*exp(-decline_harm$coefficients[1])/0.365
          Di_harm <- -decline_harm$coefficients[2]*exp(decline_harm$coefficients[1])*365/1000
          if(is.infinite(Di_harm)) Di_harm <- D_exp
          if(is.nan(Di_harm)) Di_harm <- D_exp
          D_harm <- Di_harm*exp((Cum_Prod/2)*decline_harm$coefficients[2])
        }
        if(D_harm<0.01) {
          decline_harm$coefficients[1] <- qf + D_exp*EUR_Exp
          q_2 <- -D_exp*Cum_Prod + decline_harm$coefficients[1]
          decline_harm$coefficients[1] <- log(q_2) + 10*Cum_Prod/(365*q_2)
          decline_harm$coefficients[2] <- (log(q_2)-decline_harm$coefficients[1])/Cum_Prod
          Di_harm <- -decline_harm$coefficients[2]*exp(decline_harm$coefficients[1])*365/1000
          if(is.infinite(Di_harm)) Di_harm <- D_exp
          if(is.nan(Di_harm)) Di_harm <- D_exp
          D_harm <- (Di_harm/exp(decline_harm$coefficients[1]))*q_2
        }
        EUR_Harm <- max(((log(qf)-decline_harm$coefficients[1])/decline_harm$coefficients[2]),Cum_Prod)
        EUR_Harm <- max(EUR_Harm,EUR_Exp)
        Rem_Harm <- EUR_Harm - Cum_Prod
        if(is.infinite(EUR_Harm)) EUR_Harm <- NA_real_
        #If the well hasn't produced for two years, assume it is abandoned and use the cummulative production as the EUR
        if(max(t) < (Sys.time()-60*60*24*365*2)) {
          EUR_Exp <- Cum_Prod
          EUR_Harm <- Cum_Prod
          Rem_Exp <- 0
          Rem_Harm <- 0
        }
        qi_exp <- qf+(D_exp*1000/365)*EUR_Exp
        qi_harm <- exp(log(qf)-decline_harm$coefficients[2]*EUR_Harm)
        for(j in c(1,2,3,5,10,20,30)) { 
          if(length(q)>=(j*12)){
            decline_harm <- lqs(Q[max(1,(j*12-36)):(j*12)]/1000, log(q[max(1,(j*12-36)):(j*12)]),method="lms")
            if(decline_harm$coefficients[1]>0 & decline_harm$coefficients[2]<0) {
              Di_harm <- -decline_harm$coefficients[2]*exp(decline_harm$coefficients[1])*365/1000
              if(is.finite(Di_harm)) D_j[j] <- Di_harm*exp(max(getDataPart(Q[1:(j*12)])/1000)*decline_harm$coefficients[2])
            }
          }
        }
      } else {
        #If less than 18 months, don't do anything (for now)
      }
    }  # end of the if statement to check for more than 3 data points
    D_1 <- D_j[1]
    D_2 <- D_j[2]
    D_3 <- D_j[3]
    D_5 <- D_j[5]
    D_10 <- D_j[10]
    D_20 <- D_j[20]
    D_30 <- D_j[30]
    return(c(EUR_Exp, EUR_Harm, Rem_Exp, Rem_Harm, D_exp, D_harm, qi_exp, qi_harm, qf, D_1, D_2, D_3, D_5, D_10, D_20, D_30, AlgorithmReleaseDate))
  }
}