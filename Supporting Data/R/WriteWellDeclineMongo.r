#' Read in a saved JSON file containing the decline data and write it to MongoDB.
#' Requires many external datasets to be provided: "x", "j", "products", 
#' "ResourcePlay", "calculate_areas", "temp", "well_data", "well_results", 
#' "mongo_data_base", "AlgorithmReleaseDate" and "Voronoi"
#' 
#' \code{WriteWellDeclineMongo}
#'
#' @param i iterator for each well stored in a dataframe
#' @importFrom jsonlite
#' @importFrom mongolite
#' @importFrom ffbase
#' @export

WriteWellDeclineMongo <- function(i) {
  temp_smrtone <- temp[i]
  criteria <- list(
    UWI=names(x[i]), 
    Product=products[j], 
    DataCurrencyString=substring(as.character(tail(x[[i]][,1],1)),1,7)
  )
  if(products[j]=="Gas") {
    units <- "MMcf"
  } else{
    if(products[j]=="Oil") {
      units <- "Mbbl"
    } else {
      units <- "Unknown"
    }
  } 

  eom <- function(date) {
    # date character string containing POSIXct date
    date.lt <- as.POSIXlt(strptime(date,"%Y-%m-%d"),tz="America/Denver") # add a month, then subtract a day:
    mon <- date.lt$mon + 2 
    year <- date.lt$year
    year <- year + as.integer(mon==13) # if month was December add a year
    mon[mon==13] <- 1
    iso = ISOdate(1900+year, mon, 1, hour=0, tz="America/Denver")
    result = as.POSIXct(iso) - 86400 # subtract one day
    result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
  }
      

  buf <- list(
    UWI = names(x[i]),
    Product = products[j],
    MajorProduct = tail(x[[i]]$MajorProduct,n=1),
    crs = list(type="NAD27"),
    loc = list(type="Point", coordinates=as.list(as.numeric(well_data[which(well_data$X_UWI_DISPLAY[,]==names(x[i])),][c("SURFACE_LONGITUDE", "SURFACE_LATITUDE")]))),
    Welltype = as.character(subset(well_data,X_UWI_DISPLAY==names(x[i]))$PROFILE_TYPE[,]),
    Operator = as.character(subset(well_data,X_UWI_DISPLAY==names(x[i]))$PSUM_OPERATOR_NAME[,]),
    Formation = as.character(subset(well_data,X_UWI_DISPLAY==names(x[i]))$PSUM_POOL_NAME[,]),
    PeakRate = well_results$peak_rate[match(names(x[i]), well_results$UWI[,][][(well_results$Product==products[j])[]])],
    PeakQuarter = well_results$peak_quarter[match(names(x[i]), well_results$UWI[,][][(well_results$Product==products[j])[]])],
    PeakCGR = max(x[[i]]$CGR[x[[i]]$q>0]),
    PeakGOR = max(x[[i]]$GOR[x[[i]]$q>0]),
    DataCurrency = eom(max(x[[i]]$t)),
    DataCurrencyString = substring(as.character(tail(x[[i]][,1],1)),1,7),
    LastUpdated = as.POSIXct(Sys.time(), tz="America/Denver", usetz=TRUE),
    LastUpdatedString = format(Sys.time(), tz="America/Denver", usetz=TRUE),
    MonthsOn = as.integer(length(x[[i]]$t)),
    DeclineAnalysis = list(
      list(
        Method = "SmartSingleDeclineExp", 
        EUR = as.numeric(temp_smrtone[[1]][1]), 
        Rem = as.numeric(temp_smrtone[[1]][3]),
        Units = units,
        AlgorithmReleaseDate = AlgorithmReleaseDate,
        Forecast = NULL,
        Parameters = list(
          Decline = as.numeric(temp_smrtone[][[1]][5]), 
          qi = as.numeric(temp_smrtone[][[1]][7]), 
          qf = as.numeric(temp_smrtone[][[1]][9])
        )
      ),
      list(
        Method = "SmartSingleDeclineHarm", 
        EUR = as.numeric(temp_smrtone[[1]][2]), 
        Rem = as.numeric(temp_smrtone[[1]][4]),
        Units = units,
        AlgorithmReleaseDate = AlgorithmReleaseDate,
        Forecast = NULL,
        Parameters = list(
          Decline = as.numeric(temp_smrtone[][[1]][6]), 
          qi = as.numeric(temp_smrtone[][[1]][8]), 
          qf = as.numeric(temp_smrtone[][[1]][9])
        )
      ),
      list(
        Method = "ArpsDecline", 
        EUR = as.numeric(temp_Arps[i][[1]][4]), 
        Rem = as.numeric(temp_Arps[i][[1]][5]),
        Units = units,
        AlgorithmReleaseDate = temp_Arps[i][[1]][7],
        Forecast = NULL,
        Parameters = list(
          Di = as.numeric(temp_Arps[i][[1]][3]), 
          qi = as.numeric(temp_Arps[i][[1]][1]),  
          b = as.numeric(temp_Arps[i][[1]][2]),
          qf = as.numeric(temp_Arps[i][[1]][6]),
          ProducingDays = as.numeric(temp_Arps[i][[1]][8])
        )
      ),
      list(
        Method = "EndofYearDecline",
        AlgorithmReleaseDate = AlgorithmReleaseDate,
        Parameters = list(
          D01 = as.numeric(temp_smrtone[[1]][10]), 
          D02 = as.numeric(temp_smrtone[[1]][11]), 
          D03 = as.numeric(temp_smrtone[[1]][12]), 
          D05 = as.numeric(temp_smrtone[[1]][13]), 
          D10 = as.numeric(temp_smrtone[[1]][14]), 
          D20 = as.numeric(temp_smrtone[[1]][15]), 
          D30 = as.numeric(temp_smrtone[[1]][16])
        )
      )
    )
  )
  if(!(ResourcePlay=="Undefined")) {
    buf$ResourcePlay <- ResourcePlay
  }
  if(!(ResourcePlay=="Undefined") && calculate_areas) {
    buf$Voronoi <- list(
      Area = as.numeric(Voronoi$Area[i]), 
      Perimeter = as.numeric(Voronoi$Perimeter[i])
    )
  }
  mongodb <- mongo(collection = mongo_data_base$collection, url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/", mongo_data_base$dbname,sep=""))
  try(
    mongodb$update(
      query=toJSON(criteria,auto_unbox=TRUE),
      update=paste("{\"$set\":",toJSON(buf,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),"}",sep=""),
      upsert=TRUE,
      multiple=TRUE
    )
    ,TRUE)
  rm(mongodb)
  return(head(list(criteria=criteria, document=buf)))
}         