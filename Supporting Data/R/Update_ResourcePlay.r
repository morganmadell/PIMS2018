#' Updates the "ResourcePlay" object of DeclineAnalysis array elements that have "Method" equal to "ArpsDecline"
#' 
#' \code{Update_ResourcePlay}
#'
#' @param mongo_data_base database location and authentication credentials
#' @param ResourcePlay Restrict updates to a specified resource play
#' @param CPU_count number of CPU cores to spool up to run update threads
#' @importFrom  mongolite jsonlite doSNOW snowfall
#' @export
#' @examples
Update_ResourcePlay <- function(mongo_data_base, well_list, ResourcePlay, CPU_count=1) {
  mongodb <- mongo(collection = mongo_data_base$collection, url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")  )
  
  
  Normalize_sub <- function(well) {
    mongodb <- mongo(collection = mongo_data_base$collection, url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")  )
    
    criteria <- list(
      UWI = well,
      ResourcePlay = list(
        "$ne" = ResourcePlay
      )
    )
    
    update_count <- mongodb$count(toJSON(criteria,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"))
    
    if(update_count>0) {
      iter <- mongodb$iterate(toJSON(criteria,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"))
      
      # Iterate over individual records
      while(!is.null(doc <- iter$one())){
        
        criteria2 <- list(
          UWI = doc$UWI,
          DataCurrencyString = doc$DataCurrencyString,
          Product = doc$Product
        )
        
        buf <- list(
          "$set" =
            list(
              "LastUpdated"=as.POSIXct(Sys.time(), tz="America/Denver", usetz=TRUE),
              "LastUpdatedString"=format(Sys.time(), tz="America/Denver", usetz=TRUE),
              "ResourcePlay" = ResourcePlay
            )
        )
        
        
        mongodb$update(
          query=toJSON(criteria2,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
          update=toJSON(buf,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
          upsert=TRUE,
          multiple=TRUE
        )
      }
      
      rm(criteria2)
      rm(buf, iter, doc)
    }
    rm(criteria)
    rm(mongodb)
    gc()
    return(update_count)
  }
  
  
  sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
  sfExport("Normalize_sub", "mongo_data_base", "ResourcePlay")
  sfLibrary(jsonlite)
  sfLibrary(mongolite)
  json_list <- sfSapply(as.list(well_list), Normalize_sub)
  sfStop()
  
  if(sum(json_list>0)>0) {
    return(paste0(sum(json_list>0), " ", ResourcePlay, " wells in the Mongo database had the wrong ResourcePlay.  They are now updated."))
  } else{
    return(paste0("All ", ResourcePlay, " in the Mongo database were assigned the correct ResourcePlay"))
  }
  gc()
}