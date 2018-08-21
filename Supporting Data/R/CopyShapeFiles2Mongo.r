#' Copies all shapefiles saved in a directory to a collection for the same name in a Mongo database
#' 
#' \code{CopyShapeFiles2Mongo}
#'
#' @param mongo_data_base database location and authentication credentials
#' @param directory location of subdirectory that stores the samefiles
#' @importFrom  mongolite jsonlite
#' @export
#' @examples
CopyShapeFiles2Mongo <- function(mongo_data_base, directory) {
  #library(mongolite)
  #library(jsonlite)
  mongodb <- mongo(
    collection = mongo_data_base$collection, 
    url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")
  )
  
 
  rm(mongodb)
  gc()
}