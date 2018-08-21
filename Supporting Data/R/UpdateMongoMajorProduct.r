#' Updates the "base_declines" Collection for a Mongo database fixing records with a null for MajorProduct.
#' 
#' \code{UpdateMongoMajorProduct}
#'
#' @param mongo_data_base database location and authentication credentials
#' @importFrom  mongolite jsonlite
#' @export
#' @examples
UpdateMongoMajorProduct <- function(mongo_data_base, ResourcePlay=NA_real_, CPU_count=1, limit=1000) {
  #library(mongolite)
  #library(jsonlite)
  limit <- limit
  mongodb <- mongo(
    collection = mongo_data_base$collection, 
    url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")
  )
  mongodb.well <- mongo(
    collection = "well", 
    url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")
  )
  
  
  #Get a list of wells in the specified ResourcePlay
  if(!is.na(ResourcePlay)) {
    UWIs <- mongodb$aggregate(
      pipeline = toJSON( 
        list(
          list(
            "$match" = list(
              ResourcePlay = ResourcePlay,
              MajorProduct = NULL
            )
          ),
          list(
            '$limit' = limit
          ),
          list(
            '$project' = list(
              UWI = 1
            )
          )
        ),
        auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"
      ),
      options = toJSON(
        list(
          allowDiskUse = TRUE
        ),
        auto_unbox=TRUE
      )
    )
    UWIs <- UWIs$UWI
    UWIs <- UWIs[!duplicated(UWIs)]
  }
  
  
  #If no ResourcePlay was specified, get all wells with at least one month of production
  if(is.na(ResourcePlay)) {
    UWIs <- mongodb$aggregate(
      pipeline = toJSON( 
        list(
          list(
            "$match" = list(
              MajorProduct = NULL
            )
          ),
          list(
            '$limit' = limit
          ),
          list(
            '$project' = list(
              UWI = 1
            )
          )
        ),
        auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"
      ),
      options = toJSON(
        list(
          allowDiskUse = TRUE
        ),
        auto_unbox=TRUE
      )
    )
    UWIs <- UWIs$UWI
    UWIs <- UWIs[!duplicated(UWIs)]
  }
  
  UpdateMongoMajorProduct_sub <- function(i) { 
    mongodb <- mongo(
      collection = mongo_data_base$collection, 
      url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")
    )
    mongodb.well <- mongo(
      collection = "well", 
      url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")
    )
    
    criteria <- list(
      UWI = i
    )
    
    iter <- mongodb$iterate(toJSON(criteria,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"))
    update_count <- mongodb$count(toJSON(criteria,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"))
    
    
    # Iterate over individual records
    while(!is.null(doc <- iter$one())){
      
      if(is.null(doc$MajorProduct)) {
        
        MajorProduct <- NULL
        
        if(doc$PeakCGR==0 & is.null(doc$PeakGOR)) {
          MajorProduct <- "Gas"
        }
        
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
              "MajorProduct" = MajorProduct
            )
        )
        
        
        mongodb$update(
          query=toJSON(criteria2,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
          update=toJSON(buf,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
          upsert=TRUE,
          multiple=TRUE
        )
        rm(buf, doc)
        rm(criteria2)
        rm(MajorProduct)
      }
      
    }
    rm(iter)
    rm(criteria)
    rm(mongodb,mongodb.well)
    gc()
    return(update_count)
  }
  
  
  if(length(UWIs)>0) {
    sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
    sfExport("UpdateMongoMajorProduct_sub", "mongo_data_base")
    sfLibrary(jsonlite)
    sfLibrary(mongolite)
    json_list <- sfClusterApplyLB(as.list(UWIs), function(i) {UpdateMongoMajorProduct_sub(i)})
    #json_list <- lapply(as.list(UWIs[1:10]), function(i) {UpdateMongoMajorProduct_sub(i)})
    sfStop()
    
    
    return(sum(unlist(json_list)))
  } else {
    return(0)
  }
  
  rm(mongodb, mongodb.well, UWIs)
  gc()
}