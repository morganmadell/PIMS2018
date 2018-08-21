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

SimplifiedWellDeclineList <- function(i) {
  
  AlgorithmReleaseDate <- Gas_Reserves(ReleaseDate=TRUE)
  ArpsAlgorithmReleaseDate <- Arps_Reserves(ReleaseDate=TRUE)
  mongodb <- mongo(collection = mongo_data_base$collection, url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/", mongo_data_base$dbname,sep=""))
  ForceCalculate <- FALSE
  
  criteria01 <- list(
    UWI=names(x[i]), 
    Product=products[j], 
    DataCurrencyString=substring(as.character(tail(x[[i]][,1],1)),1,7)
  )
  
  project01 <- list(
    DeclineAnalysis = "$DeclineAnalysis"
  )
  
  Record <- mongodb$aggregate(
    toJSON(
      list(
        list("$match" = criteria01),
        list("$project" = project01)
      ),
      auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"
    )
  )
  
  if(length(Record)==0) {
    ForceCalculate <- TRUE
  } else {
    if(length(subset(Record$DeclineAnalysis[[1]], Method=="SmartSingleDeclineExp"))==0) {
      ForceCalculate <- TRUE
    } else {
      if(subset(Record$DeclineAnalysis[[1]], Method=="SmartSingleDeclineExp")[,5]<(AlgorithmReleaseDate-.001) | is.na(subset(Record$DeclineAnalysis[[1]], Method=="SmartSingleDeclineExp")[,5])) {
        ForceCalculate <- TRUE
      } else {
      if(length(subset(Record$DeclineAnalysis[[1]], Method=="ArpsDecline"))==0) {
        ForceCalculate <- TRUE
      } else {
        if(subset(Record$DeclineAnalysis[[1]], Method=="ArpsDecline")[,5]<(ArpsAlgorithmReleaseDate-.001) | is.na(subset(Record$DeclineAnalysis[[1]], Method=="ArpsDecline")[,5])) {
          ForceCalculate <- TRUE
        }
      }
    }
  }
  }
   
  rm(Record, criteria01, project01)
  rm(mongodb, ArpsAlgorithmReleaseDate, AlgorithmReleaseDate)
  gc()
  return(ForceCalculate)
}         