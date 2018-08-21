#' Updates the "recent" Collection for a Mongo database that follows the GLJ "base_decline" Schema.
#' 
#' \code{UpdateMongoRecentCollection}
#'
#' @param mongo_data_base database location and authentication credentials
#' @importFrom  mongolite jsonlite
#' @export
#' @examples
UpdateMongoRecentCollection <- function(mongo_data_base, ResourcePlay=NA_real_, CPU_count=1, PurgeRecentCollection=FALSE) {
  #library(mongolite)
  #library(jsonlite)
  mongodb <- mongo(
    collection = mongo_data_base$collection, 
    url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")
  )
  mongodb.recent <- mongo(
    collection = "recent", 
    url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")  
    )
  
  
  #Get a list of wells in the specified ResourcePlay
  if(!is.na(ResourcePlay)) {
    UWIs <- mongodb$find(
      query = toJSON(
        list(
          ResourcePlay = ResourcePlay
        ),
        auto_unbox=TRUE
      ),
      fields = toJSON(
        list(
          UWI = 1
        ),
        auto_unbox=TRUE
      )
    )
    UWIs <- UWIs$UWI
    UWIs <- UWIs[!duplicated(UWIs)]
  }
  
  
  #If no ResourcePlay was specified, get all wells with at least one month of production
  if(is.na(ResourcePlay)) {
    UWIs <- mongodb$find(
      query = toJSON(
        list(
          MonthsOn = list(
            "$gt" = 0
          )
        ),
        auto_unbox=TRUE
      ),
      fields = toJSON(
        list(
          UWI = 1
        ),
        auto_unbox=TRUE
      )
    )
    UWIs <- UWIs$UWI
    UWIs <- UWIs[!duplicated(UWIs)]
  }
  
  
    if(FALSE) {
  mongodb$aggregate(
    pipeline = toJSON(
      list(
        list(
          "$match" = list(
            ResourcePlay = list(
              '$exists' = 1
            )
          )
        ),
        list(
          "$sort" = list(
            "DataCurrencyString" = -1
          )
        ),
        list(
          '$group' = list(
            "_id" = list(
              UWI = "$UWI",
              Product = "$Product"
              ),
            BaseDeclineID = list(
              "$first" ="$_id"
            ),
            DataCurrencyString = list(
              "$first" ="$DataCurrencyString"
            ),
            Product = list(
              "$first" ="$Product"
            ),
            MajorProduct = list(
              "$first" ="$MajorProduct"
            ),
            UWI = list(
              "$first" ="$UWI"
            )
          )
        ),
        list(
          "$out" = "recent"
        )
      ),
      auto_unbox=TRUE
    ),
    pagesize = 1000
  )
    }
  
  
  
  
  
  UpdateMongoRecentCollection_sub <- function(i) { 
    mongodb <- mongo(
      collection = mongo_data_base$collection, 
      url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")
    )
    mongodb.recent <- mongo(
      collection = "recent", 
      url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")
    )
    
    buf <- mongodb$aggregate(
      pipeline = toJSON(
        list(
          list(
            "$match" = list(
              UWI = i
            )
          ),
          list(
            "$project" = list(
              UWI = 1,
              Product = 1,
              DataCurrencyString = 1
            )
          ),
          list(
            "$sort" = list(
              "DataCurrencyString" = -1
            )
          ),
          list(
            '$group' = list(
              "_id" = list(
                UWI = "$UWI",
                Product = "$Product"
              ),
              BaseDeclineID = list(
                "$first" ="$_id"
              ),
              DataCurrencyString = list(
                "$first" ="$DataCurrencyString"
              ),
              Product = list(
                "$first" ="$Product"
              ),
              MajorProduct = list(
                "$first" ="$MajorProduct"
              ),
              UWI = list(
                "$first" ="$UWI"
              )
            )
          )
        ),
        auto_unbox=TRUE)
    )
    
    if(FALSE) {
    #Only keep the record in which the calculated product (now named "_id") matches the MajorProduct (now named "Product")
    if(!is.na(any(buf["_id"] == buf["Product"])) && any(buf["_id"] == buf["Product"])) {
      buf <- buf[buf["_id"] == buf["Product"],]
    }
    
    #Check if MajorProduct (now named "Product") was defined, if not, then set it equal to the product (now named "_id")
    if(is.na(buf$Product)) {
      buf$Product <- buf["_id"][[1]]
    }
    
    #remove the _id record... it will cause problems
    buf <- buf[ , !(names(buf) %in% c("_id"))]
    }
    
    
    #reformat buf so our update command will accept it
    buf2 <- as.list(buf)
    if(FALSE) {
    buf2$loc <- list(
      type = buf2$loc$type,
      coordinates = buf2$loc$coordinates[[1]]
    )
    }
    buf2 <- list(
      "$set" = buf2
    )
    
    
    criteria <- list(
      UWI = i
    )
    
    #remove duplicate UWIs... might do later, if necessary
    #http://stackoverflow.com/questions/14184099/fastest-way-to-remove-duplicate-documents-in-mongodb
    
    #update the record in the well collection
    mongodb.recent$update(
      query=toJSON(criteria,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
      update=toJSON(buf2,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
      upsert=TRUE,
      multiple=TRUE
    )
    rm(criteria)
    rm(buf,buf2)
    rm(mongodb, mongodb.recent)
  }
  
  
  
  
  if(length(UWIs)>0) {
    sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
    sfExport("UpdateMongoRecentCollection_sub", "mongo_data_base")
    sfLibrary(jsonlite)
    sfLibrary(mongolite)
    json_list <- sfClusterApplyLB(as.list(UWIs), function(i) {UpdateMongoRecentCollection_sub(i)})
    #json_list <- lapply(as.list(UWIs[1:10]), function(i) {UpdateMongoRecentCollection_sub(i)})
    sfStop()
    
    #Remove all UWIs in the well collection that aren't in the list we just pulled
    if(PurgeRecentCollection) {
      buf <- list(
        ResourcePlay = ResourcePlay,
        UWI = list(
          "$nin" = UWIs 
        )
      )
      mongodb.recent$remove(
        query=toJSON(buf,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
        multiple=TRUE
      )
      rm(buf)
    }
    
    return(sum(unlist(json_list)))
  } else {
    return(0)
  }
  
  rm(mongodb, mongodb.recent, UWIs)
  gc()
}