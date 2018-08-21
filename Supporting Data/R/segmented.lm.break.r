#' Returns a continuous linear model of form "formula" on data "x" 
#' 
#' \code{segmented.lm.break} returns a continuous linear model of form "formula" 
#' on data "x", segmented at internally calculated breakpoints "b"
#'
#' @param formula linear model of form "formula"
#' @param x data frame to parse
#' @importFrom segmented segmented
#' @export segmented.lm.break
#' @examples
#' formula <- q~Q
#' data <- data.frame(q=5*(100:1),Q=20*(1:100),qf=rep(25,100))
#' segmented.lm.break(formula, data)
segmented.lm.break <- function(formula, x){
  #formula should be a form like q~Q
  x <- subset(x,q>0)
  b <- break.lm(formula, x)
  return(segmented(lm(formula, data=x), seg.Z = ~Q, psi=x$Q[b]))
}