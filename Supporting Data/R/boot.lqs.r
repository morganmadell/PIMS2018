#' Returns a function that does a quantile regression linear model to a random sample of a dataset... ie do a bootstrap fit
#' 
#' \code{boot.lqs}
#'
#' @param formula linear model of form "formula", generally like q~Q
#' @param data data frame to parse
#' @param ... additional paramaters to pass on
#' @importFrom MASS lqs
#' @export
#' @examples
#' formula <- q~Q
#' data <- data.frame(q=5*(100:1),Q=20*(1:100),qf=rep(25,100))
#' boot.lqs(formula, data)
boot.lqs <- function(formula, data, ...){
  function(){
    lqs(formula=formula, 
        data=data[sample(nrow(data), replace=TRUE),], method="lms", ...)
  }
}