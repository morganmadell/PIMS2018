#' Calculates linear flow reserves with uncertainty using bootstrap method
#' 
#' \code{Boot.Watten.Reserves}
#' All iterations that give a cumulative production less than the actual cum are rejected 
#' and a replacement iteration is added.
#'
#' @param x data frame to parse
#' @param iters number of iteration to use for bootstrap estiamtes
#' @export
#' @examples
#' data <- data.frame(q=5*(10:1),t=20*(1:10),qf=rep(25,10))
#' iters <- 5
#' Boot.Watten.Reserves(data, iters)
Boot.Watten.Reserves <- function(x, iters){
  x <- subset(x,q>0)
  bstrap_iters <- iters
  bstrap <- rep(NA_real_, bstrap_iters)
  
  # boot.watten builds an enclosure of the data "x" and a function that returns a bootstrapped replicate of an harmonic decline (or the cum production to date if it's the larger) on the data
  boot.watten <- function(x, ...){
    function(){
      qf <- max(x$qf)
      Qcum <- max(x$Q)
      temp <- boot.lm(1/q ~ t, x)()$coef
      EUR <- as.numeric(
        x$Q[1] + 
        (2/(temp[2]^2))*(
          (1/qf-1/x$q[1]) + 
          temp[1]*(log(qf)-log(x$q[1]))
          )
        )
      return(max(EUR,Qcum))
    }
  }
  
  # Boot.Harm is an unargumented function that returns a bootstrapped replicate of an harmonic decline
  Boot.Watten <- boot.watten(x)
  
  if(length(x$q)<3) {
    # If only three producing months, don't do much for now
  } else {
    # Check if the well has more than 3 producing months before doing a decline
    if(length(x$q)>3) {
      bstrap <- sapply(X=1:bstrap_iters, FUN=function(x) Boot.Watten())
    }
  }
  return(bstrap)
}