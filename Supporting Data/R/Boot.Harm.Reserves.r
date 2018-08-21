#' Calculates harmonic reserves with uncertainty using bootstrap method
#' 
#' \code{Boot.Harm.Reserves}
#' All iterations that give a cumulative production less than the actual cum are rejected and a replacement iteration is added
#'
#' @param x data frame to parse
#' @param iters number of iteration to use for bootstrap estiamtes
#' @export
#' @examples
#' data <- data.frame(q=5*(100:1),Q=20*(1:100),qf=rep(25,100))
#' iters <- 5
#' Boot.Harm.Reserves(data, iters)
Boot.Harm.Reserves <- function(x, iters){
  x <- subset(x,q>0)
  bstrap_iters <- iters
  bstrap <- rep(NA_real_, bstrap_iters)
  
  # boot.harm builds an enclosure of the data "x" and a function that returns a bootstrapped replicate of an harmonic decline (or the cum production to date if it's the larger) on the data
  boot.harm <- function(x, ...){
    function(){
      qf <- max(x$qf)
      Qcum <- max(x$Q)
      temp <- boot.lm(log(q) ~ Q, x)()$coef
      EUR <- as.numeric((log(qf)-temp[1])/temp[2])
      return(max(EUR,Qcum))
    }
  }
  
  # Boot.Harm is an unargumented function that returns a bootstrapped replicate of an harmonic decline
  Boot.Harm <- boot.harm(x)
  
  if(length(x$q)<3) {
    # If only three producing months, don't do much for now
  } else {
    # Check if the well has more than 4 producing months before doing a decline
    if(length(x$q)>4) {
      bstrap <- sapply(X=1:bstrap_iters, FUN=function(x) Boot.Harm())
    }
  }
  return(bstrap)
}