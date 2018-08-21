#' Returns the peak rolling-quarter rate.
#' 
#' \code{Peak_Quarter}
#'
#' @param x data frame to parse
#' @importFrom zoo rollmean
#' @export
#' @examples
#' data <- data.frame(X_UWI_DISPLAY="Well", GAS_CAL_DAY=5*(10:1))
#' Peak_Quarter(data)
Peak_Quarter <- function(x=NA_real_) {
  if(length(na.omit(x))>2) {
    return(max(rollmean(na.omit(x), 3)))
  } else {
    return(NA_real_)
  }
}