#' Returns an estimate of a gas well's abandonment rate (Mcf/d) based on the depth of the completed interval (m).
#' 
#' \code{Abandon_Rate_Gas}
#'
#' @param mTVD depth of completed interval
#' @export
#' @examples
#' depth <- 953.2
#' Abandon_Rate_Gas(depth)
Abandon_Rate_Gas <- function(mTVD) {
  return(ceiling(mTVD/200.0)*5.0)
}