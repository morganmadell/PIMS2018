#' Converts HPDI source data into a data frame with the variables we need to perform a gas decline.
#' 
#' \code{HPDI_Cal_Rates}
#'
#' @param df data frame to parse, must contain the rows GAS, OIL_BT and WATER
#' @export
#' @examples
#' data <- data.frame(PROD_DATE=seq(as.POSIXct("2000/1/1"), by = "month", length.out = 10), GAS=5*(10:1), OIL_BT=5*(10:1), WATER=5*(10:1))
#' HPDI_Cal_Rates(data)
HPDI_Cal_Rates <- function(df) {
  days <- days_in_month(df$PROD_DATE)
  data.frame(GAS_CAL_DAY=(df$GAS)/days, OIL_CAL_DAY=(df$OIL_BT)/days, WATER_CAL_DAY=(df$WATER)/days)
}