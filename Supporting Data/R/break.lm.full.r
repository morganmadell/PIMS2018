#' Returns the full analysis of breakpoints (no more than 3) for a linear model of form "formula" on data "x".
#' 
#' \code{break.lm.full}
#'
#' @param formula linear model of form "formula", generally like q~Q
#' @param x data frame to parse
#' @importFrom strucchange breakpoints
#' @export
#' @examples
#' formula <- q~Q
#' data <- data.frame(q=5*(100:1),Q=20*(1:100),qf=rep(25,100))
#' #To show the fitted regression lines, which are discontinuous, ...
#' #require(strucchange)
#' #bp.seat <- break.lm.full(formula, data)
#' #plot(data$Q,data$q)
#' #lines(data$Q,fitted(lm(q ~ breakfactor(bp.seat)/Q - 1, data = data)),col="red")
break.lm.full <- function(formula, x){
  #the hpc parameter allows for specifications of high performance computing support
  x <- subset(x,q>0)
  #return(breakpoints(formula, data = x, h = 0.1, breaks=2, hpc="none"))
  return(breakpoints(formula, data = x, h = 4, breaks=1, hpc="none"))
}