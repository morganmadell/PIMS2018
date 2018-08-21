#' Returns a function that fits a linear model to a random sample of a dataset... ie do a bootstrap fit
#' 
#' \code{boot.lm}
#'
#' @param formula linear model of form "formula", generally like q~Q
#' @param data data frame to parse
#' @param ... additional paramaters to pass on
#' @export
#' @examples
#' formula <- q~Q
#' data <- data.frame(q=5*(100:1),Q=20*(1:100),qf=rep(25,100))
#' boot.lm(formula, data)
boot.lm <- function(formula, data, ...){
  function(){
    lm(formula=formula, 
       data=data[sample(nrow(data), replace=TRUE),], ...)
  }
}