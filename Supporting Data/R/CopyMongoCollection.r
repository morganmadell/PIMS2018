#' Updates the "well" Collection for a Mongo database that follows the GLJ "base_decline" Schema.
#' 
#' \code{CopyMongoCollection}
#'
#' @param mongo_data_base_in source database location and authentication credentials
#' @param mongo_data_base_out target database location and authentication credentials
#' @importFrom  mongolite jsonlite
#' @export
#' @examples
#' 
#' 
CopyMongoCollection <- function(mongo_data_base_in, mongo_data_base_out) {
  #library(mongolite)
  #library(jsonlite)
  if(FALSE) {
    mongo_data_base_in <- data.frame(host="Empty",  rsname="Empty", dbname="Empty", timeout=NA_integer_, user="Empty", pwd="Empty", collection="Empty")
    mongo_data_base_in$host <- "mongodb-test.corp.gljpc.com:27017"
    mongo_data_base_in$rsname <- "gljdata_rs01"
    mongo_data_base_in$dbname <- "DeclineDashboard"
    mongo_data_base_in$timout <- 1000
    mongo_data_base_in$user <- "MMorgan"
    mongo_data_base_in$pwd <- "MMorgan"
    mongo_data_base_in$collection <- "base_declines"
    
    mongo_data_base_in <- data.frame(host="Empty",  rsname="Empty", dbname="Empty", timeout=NA_integer_, user="Empty", pwd="Empty", collection="Empty")
    mongo_data_base_in$host <- "testDeclineDashboard.corp.gljpc.com:27017"
    mongo_data_base_in$rsname <- "gljdata_rs01"
    mongo_data_base_in$dbname <- "DeclineDashboard"
    mongo_data_base_in$timout <- 1000
    mongo_data_base_in$user <- "MMorgan"
    mongo_data_base_in$pwd <- "MMorgan"
    mongo_data_base_in$collection <- "base_declines"
    
    mongo_data_base_out <- data.frame(host="Empty",  rsname="Empty", dbname="Empty", timeout=NA_integer_, user="Empty", pwd="Empty", collection="Empty")
    mongo_data_base_out$host <- "mongodb-test.corp.gljpc.com:27017"
    mongo_data_base_out$rsname <- "gljdata_rs01"
    mongo_data_base_out$dbname <- "DeclineDashboard"
    mongo_data_base_out$timout <- 1000
    mongo_data_base_out$user <- "MMorgan"
    mongo_data_base_out$pwd <- "MMorgan"
    mongo_data_base_out$collection <- "base_declines_copy"
    
    mongo_data_base_out <- data.frame(host="Empty",  rsname="Empty", dbname="Empty", timeout=NA_integer_, user="Empty", pwd="Empty", collection="Empty")
    mongo_data_base_out$host <- "db.gljdata.com:27017"
    mongo_data_base_out$rsname <- "gljdata_rs01"
    mongo_data_base_out$dbname <- "gljdata_test"
    mongo_data_base_out$timout <- 1000
    mongo_data_base_out$user <- "mike"
    mongo_data_base_out$pwd <- "glj"
    mongo_data_base_out$collection <- "base_declines"
    
    tmp <- tempfile(tmpdir="c:/temp")
  }
  
  
  mongodb_in <- mongo(
    collection = mongo_data_base_in$collection, 
    url = paste("mongodb://", mongo_data_base_in$user, ":", mongo_data_base_in$pwd, "@", mongo_data_base_in$host, "/",mongo_data_base_in$dbname,sep="")
  )
  mongodb_out <- mongo(
    collection = mongo_data_base_out$collection, 
    url = paste("mongodb://", mongo_data_base_out$user, ":", mongo_data_base_out$pwd, "@", mongo_data_base_out$host, "/",mongo_data_base_out$dbname,sep="")
  )
  
    
  #mongodb_in$export(mongodb_out,bson=TRUE)
  mongodb_in$export(file(tmp), bson=TRUE)
  mongodb_out$import(file(tmp), bson=TRUE)
  #mongodb_out$find(toJSON({}))
  
  rm(tmp)
  rm(mongodb_in, mongodb_out)
  gc()
}