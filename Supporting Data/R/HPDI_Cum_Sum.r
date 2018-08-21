#' Returns a dataframe with running cumulative gas, oil and water.
#' 
#' \code{HPDI_Cum_Sum}
#'
#' @param df data frame to parse, must contain the rows GAS, OIL_BT and WATER
#' @export
#' @examples
#' data <- data.frame(GAS=5*(10:1), OIL_BT=5*(10:1), WATER=5*(10:1))
#' HPDI_Cum_Sum(data)
HPDI_Cum_Sum <- function(df) {
  data.frame(CUM_GAS=cumsum(pmax(df$GAS,0)), CUM_OIL_BT=cumsum(pmax(df$OIL_BT,0)), CUM_WATER=cumsum(pmax(df$WATER,0)))
}