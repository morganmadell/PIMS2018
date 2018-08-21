#' Returns the index of the optimum breakpoints (no more than 3) for a linear model of form "formula" on data "x".
#' 
#' \code{break.lm}
#'
#' @param formula linear model of form "formula", generally like q~Q
#' @param x data frame to parse
#' @importFrom strucchange breakpoints
#' @export
#' @examples
#' formula <- q~Q
#' data <- data.frame(q=5*(100:1),Q=20*(1:100),qf=rep(25,100))
#' break.lm(formula, data)
break.lm <- function(formula, x){
  #the hpc parameter allows for specifications of high performance computing support
  x <- subset(x,q>0)
  #return(breakpoints(formula, data = x, h = 0.1, breaks=2, hpc="none")$breakpoints)
  return(breakpoints(formula, data = x, h = 4, breaks=1, hpc="none")$breakpoints)
}