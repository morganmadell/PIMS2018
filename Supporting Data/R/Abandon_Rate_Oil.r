#' Returns an estimate of an oil well's abandonment rate (bbl/d) based on the depth of the completed interval (m)
#' 
#' \code{Abandon_Rate_Gas}
#'
#' @param mTVD depth of completed interval
#' @export
#' @examples
#' depth <- 953.2
#' Abandon_Rate_Oil(depth)
Abandon_Rate_Oil <- function(mTVD) {
  return(ceiling(mTVD/8000.0)*2.0)
}