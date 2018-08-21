#' Returns a data frame with the UWI and peak monthly gas rate.
#' 
#' \code{Peak_Rate}
#'
#' @param x data frame to parse
#' @return data frame containing the UWI and peak monthly gas rate of \code{x}
#' @export
#' @examples
#' data <- data.frame(X_UWI_DISPLAY="Well", GAS_CAL_DAY=5*(10:1))
#' Peak_Rate(data)
Peak_Rate <- function(x) {
  data.frame(X_UWI_DISPLAY=as.character(x$X_UWI_DISPLAY[1]), GAS_CAL_DAY=max(x$GAS_CAL_DAY))
}