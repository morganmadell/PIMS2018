#' Read in a saved JSON file containing the decline data and write it to MongoDB.
#' 
#' \code{WriteJSONDeclineMongo}
#'
#' @param x name of JSON file to parse and write to MongoDB
#' @importFrom jsonlite
#' @importFrom mongolite
#' @export

WriteJSONDeclineMongo <- function(x) {
  #tmp <- fromJSON(paste("[",readLines(list_json_files[2])[1],"]",sep=""),simplifyVector=FALSE)
  tmp <- fromJSON(paste("[",x,"]",sep=""),simplifyVector=FALSE)
  try(crit <- toJSON(tmp[[1]]$criteria),TRUE)
  try(docu <- toJSON(tmp[[1]]$document),TRUE)
  #print(docu)
  if(exists("crit") & exists("docu")) {
    #mongo.update(
    #  mongo = mongodb, 
    #  ns = mongo_data_base$collection, 
    #  criteria = mongo.bson.from.list(crit), 
    #  objNew = list('$set'=docu), 
    #  mongo.update.upsert
    #  )
    #mongodb2 <- mongo(
    #  collection = mongo_data_base$collection, 
    #  url = paste("mongodb://", "Admin:%JTjqPJ%GC", "@", mongo_data_base$host, "/", mongo_data_base$dbname,sep="")
    #  )
    mongodb <- mongo(collection = mongo_data_base$collection, url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/", mongo_data_base$dbname,sep=""))
    #mongodb$count(
    #  query=substr(as.character(toJSON(tmp$criteria,pretty=TRUE)),2,nchar(as.character(toJSON(tmp$criteria,pretty=TRUE)))-1)
    #)
    #mongodb$count(
    #  query=substr(toJSON(crit),2,nchar(toJSON(crit))-1)
    #)
    #mongodb$update(
    #  query=substr(toJSON(crit),2,nchar(toJSON(crit))-1),
    #  update=paste("{\"$set\":",substr(toJSON(docu),2,nchar(toJSON(docu))-1),"}",sep=""),
    #  upsert=TRUE,
    #  multiple=TRUE
    #)
    #query=substr(crit,2,nchar(crit)-1),
    #update=paste("{\"$set\":",substr(docu,2,nchar(docu)-1),"}",sep=""),
    try(
      mongodb$update(
        query=crit,
        update=paste("{\"$set\":",docu,"}",sep=""),
        upsert=TRUE,
        multiple=TRUE
      )
      ,TRUE)
  }
  rm(mongodb)
  gc()
}
