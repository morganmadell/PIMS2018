#' Updates the "Forecast" object of DeclineAnalysis array elements that have "Method" equal to "ArpsDecline"
#' 
#' \code{Arps_Forecasts}
#'
#' @param mongo_data_base database location and authentication credentials
#' @param CPU_count number of CPU cores to spool up to run update threads
#' @param ResourcePlay Restrict updates to a specified resource play
#' @param CustomForecastDate Not yet implemented
#' @importFrom  mongolite jsonlite aRpsDCA doSNOW snowfall
#' @export
#' @examples
Arps_Forecasts <- function(mongo_data_base, CPU_count=1, ResourcePlay=NA, CustomForecastDate=NA, limit=1000) {
  #library(mongolite)
  #library(jsonlite)
  #library(aRpsDCA)
  #library(doSNOW)
  #library(snowfall)
  ArpsAlgorithmReleaseDate <- Arps_Reserves(ReleaseDate=TRUE)
  #limit <- 1000
  mongodb <- mongo(collection = mongo_data_base$collection, url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")  )

  
  
  
  eom <- function(date) {
    # date character string containing POSIXct date
    date.lt <- as.POSIXlt(date,tz="MST") # add a month, then subtract a day:
    mon <- date.lt$mon + 2 
    year <- date.lt$year
    year <- year + as.integer(mon==13) # if month was December add a year
    mon[mon==13] <- 1
    iso = ISOdate(1900+year, mon, 1, hour=0, tz="MST")
    result = as.POSIXct(iso) - 86400 # subtract one day
    result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
  }
  
  
  Arps_Forecasts_sub <- function(i) {
    if(is.na(CustomForecastDate)) {
      ForecastDate = Records$DataCurrency[i]
      }
    mongodb <- mongo(collection = mongo_data_base$collection, url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/", mongo_data_base$dbname,sep=""))
    
    #Build a 50 year forecast of times, starting at the forecast date and incrementing to the end of every month... return the result in units of days
    t <- cumsum(as.numeric(diff(eom(seq(from=as.POSIXct(strptime(paste0(Records$DataCurrencyString[i],"-01"),"%Y-%m-%d"), tz="MST", usetz=TRUE), by="month",length.out=50*12+1))))) + Records$DeclineAnalysis[[i]]$Parameters$ProducingDays
    
    Di <- Records$DeclineAnalysis[[i]]$Parameters$Di/100
    qi <- Records$DeclineAnalysis[[i]]$Parameters$qi
    b <- Records$DeclineAnalysis[[i]]$Parameters$b
    qf <- Records$DeclineAnalysis[[i]]$Parameters$qf
    
    q <- hyperbolic.q(qi, Di, b, t)
    Q <- q * as.numeric(diff(eom(seq(from=as.POSIXct(strptime(paste0(Records$DataCurrencyString[i],"-01"),"%Y-%m-%d"), tz="MST", usetz=TRUE), by="month",length.out=50*12+1))))
    #Np <- cumsum(Q) + (Records$DeclineAnalysis[[i]]$EUR-Records$DeclineAnalysis[[i]]$Rem)*1000
    Np <- hyperbolic.Np(qi,Di,b,t)
    
    criteria <- list(
      UWI=Records$UWI[i],
      DataCurrencyString=Records$DataCurrencyString[i],
      Product=Records$Product[i],
      DeclineAnalysis = list(
        '$elemMatch' = list(
          Method = "ArpsDecline",
          Rem = list(
            "$gt" = 0
          )
        )
      )
    )
    
    buf <- list(
      "$set" =
        list(
          "LastUpdated"=as.POSIXct(Sys.time(), tz="America/Denver", usetz=TRUE),
          "LastUpdatedString"=format(Sys.time(), tz="America/Denver", usetz=TRUE),
          "DeclineAnalysis.$.Forecast" = list(
            rate = q[q>qf],
            time = t[q>qf],
            volume = Q[q>qf],
            cumulative = Np[q>qf],
            month = eom(seq(from=as.POSIXct(strptime(paste0(Records$DataCurrencyString[i],"-01"),"%Y-%m-%d"), tz="MST", usetz=TRUE), by="month",length.out=50*12+1))[-1][q>qf]
          ),
          "DeclineAnalysis.$.AlgorithmReleaseDate" = ArpsAlgorithmReleaseDate
        )
    )
    
    #try(
    mongodb$update(
      query=toJSON(criteria,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
      update=toJSON(buf,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
      upsert=TRUE
    )
    #,TRUE)
    rm(criteria)
    rm(buf)
    rm(mongodb)
    rm(t,Di,qi,b,qf,Np,Q,q)
    gc()
  }
  
  
  NRAForecasts <- function() {
    #If there is no remaining gas, set the forecast equal to 0:  NB, this update only sets the first element in the array that has REM=0
    mongodb$update(
      query = toJSON(
        list(
          DeclineAnalysis = list(
            '$elemMatch' = list(
              Rem = 0,
              Forecast = list(
                "$ne" = 0
              )
            )
          ),
          "limit" = limit
        ),
        auto_unbox=TRUE,
        null="null",
        na="null"
      ),
      update = toJSON(
        list(
          "$set" =
            list(
              "LastUpdated"=as.POSIXct(Sys.time(), tz="America/Denver", usetz=TRUE),
              "LastUpdatedString"=format(Sys.time(), tz="America/Denver", usetz=TRUE),
              "DeclineAnalysis.$.Forecast" = list(
                0
              )
            )
        ),
        auto_unbox=TRUE,
        POSIXt="mongo",
        null="null",
        na="null"
      ),
      upsert=TRUE,
      multiple=TRUE
    )
  }
  
  #If the remaining gas is set to null, set the forecast equal to null:  NB, this update only sets the first element in the array that has REM=NULL
  if(FALSE) {
    mongodb$update(
      query = toJSON(
        list(
          DeclineAnalysis = list(
            '$elemMatch' = list(
              Rem = list(
                "$exists" = TRUE,
                "$not" = list(
                  "$gt" = 0
                )
              ),
              Forecast = list(
                "$ne" = 0
              )
            )
          )
        ),
        auto_unbox=TRUE,
        null="null",
        na="null"
      ),
      update = toJSON(
        list(
          "$set" =
            list(
              "LastUpdated"=as.POSIXct(Sys.time(), tz="America/Denver", usetz=TRUE),
              "LastUpdatedString"=format(Sys.time(), tz="America/Denver", usetz=TRUE),
              "DeclineAnalysis.$.Forecast" = 0
            )
        ),
        auto_unbox=TRUE,
        POSIXt="mongo",
        null="null",
        na="null"
      ),
      upsert=TRUE,
      multiple=TRUE
    )
  }
  
  
  
  
  
  if(!is.na(ResourcePlay)) {
  Records <- mongodb$aggregate(
    pipeline = toJSON(
      list(
        list(
          "$match" = list(
            "$or" = list(
              list(
                ResourcePlay = ResourcePlay,
                DeclineAnalysis = list(
                  '$elemMatch' = list(
                    Method = "ArpsDecline",
                    Rem = list(
                      "$gt" = 0
                    ),
                    Parameters.ProducingDays = list(
                      "$exists" = TRUE
                    ),
                    AlgorithmReleaseDate = list(
                      "$lt" = ArpsAlgorithmReleaseDate
                    )
                  )
                )
              ),
              list(
                ResourcePlay = ResourcePlay,
                DeclineAnalysis = list(
                  '$elemMatch' = list(
                    Method = "ArpsDecline",
                    Rem = list(
                      "$gt" = 0
                    ),
                    Parameters.ProducingDays = list(
                      "$exists" = TRUE
                    ),
                    Forecast.rate = list(
                      "$exists" = FALSE
                    )
                  )
                )
              )
            )
          )
        ),
        list(
          "$limit" = limit*2
        ),
        list(
          "$project" = list (
            MonthsOn = "$MonthsOn",
            DeclineAnalysis = list(
              "$filter" = list (
                input = "$DeclineAnalysis",
                as = "analysis",
                cond = list (
                  "$eq" = list(
                    "$$analysis.Method", 
                    "ArpsDecline"
                  )
                )
              )
            ),
            UWI = "$UWI",
            DataCurrencyString = "$DataCurrencyString",
            DataCurrency = "$DataCurrency",
            Product = "$Product"
            
          )
        ),
        list(
          "$sort" = list(
            "DataCurrency" = -1,
            "MonthsOn" = -1
          )
        ),
        list(
          "$limit" = limit
        )
        ),
      auto_unbox=TRUE
    ),
    options = toJSON(
      list(
        allowDiskUse = TRUE
      ),
      auto_unbox=TRUE
    )
  )
  } else {
    Records <- mongodb$aggregate(
      pipeline = toJSON(
        list(
        list(
          list(
            "$match" = list(
              "$or" = list(
                list(
                  DeclineAnalysis = list(
                    '$elemMatch' = list(
                      Method = "ArpsDecline",
                      Rem = list(
                        "$gt" = 0
                      ),
                      Parameters.ProducingDays = list(
                        "$exists" = TRUE
                      ),
                      AlgorithmReleaseDate = list(
                        "$lt" = ArpsAlgorithmReleaseDate
                      )
                    )
                  )
                ),
                list(
                  DeclineAnalysis = list(
                    '$elemMatch' = list(
                      Method = "ArpsDecline",
                      Rem = list(
                        "$gt" = 0
                      ),
                      Parameters.ProducingDays = list(
                        "$exists" = TRUE
                      ),
                      Forecast.rate = list(
                        "$exists" = FALSE
                      )
                    )
                  )
                )
              )
            )
          ),
          list(
            "$project" = list (
              MonthsOn = "$MonthsOn",
              DeclineAnalysis = list(
                "$filter" = list (
                  input = "$DeclineAnalysis",
                  as = "analysis",
                  cond = list (
                    "$eq" = list(
                      "$$analysis.Method", 
                      "ArpsDecline"
                    )
                  )
                )
              ),
              UWI = "$UWI",
              DataCurrencyString = "$DataCurrencyString",
              DataCurrency = "$DataCurrency",
              Product = "$Product"
              
            )
          ),
          list(
            "$limit" = limit*2
          ),
          list(
            "$sort" = list(
              "DataCurrencyString" = -1,
              "MonthsOn" = -1
            )
          ),
          list(
            "$limit" = limit
          )
        ),
        list(
          allowDiskUse=TRUE
        )
        ),
        auto_unbox=TRUE
      ),
      options = toJSON(
        list(
          allowDiskUse = TRUE
        ),
        auto_unbox=TRUE
      )
    )  
  }
  
  
  
  
  #$filter	Selects a subset of the array to return an array with only the elements that match the filter condition.
  
  
  if(is.na(ResourcePlay)) { try(NRAForecasts(),TRUE) }
  if(length(Records$UWI)>0) {
    
    #lapply(1:length(Records$ObjectID), Arps_Forecasts_sub)
    
    sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
    sfExport("Arps_Forecasts_sub", "eom", "mongo_data_base", "Records", "CustomForecastDate", "ArpsAlgorithmReleaseDate")
    sfLibrary(jsonlite)
    sfLibrary(mongolite)
    sfLibrary(aRpsDCA)
    json_list <- sfSapply(1:length(Records$UWI), Arps_Forecasts_sub)
    #json_list <- sfClusterApplyLB(1:length(Records$UWI), Arps_Forecasts_sub)
    #json_list <- sfClusterApplyLB(1:20, Arps_Forecasts_sub)
    sfStop()
    
        
    return(Records$UWI)
  } else {
    return()
  }
  
  
  
  rm(Records, ArpsAlgorithmReleaseDate, limit)
  rm(mongodb)
  gc()
}