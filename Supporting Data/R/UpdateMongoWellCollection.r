#' Updates the "well" Collection for a Mongo database that follows the GLJ "base_decline" Schema.
#' 
#' \code{UpdateMongoWellCollection}
#'
#' @param mongo_data_base database location and authentication credentials
#' @importFrom  mongolite jsonlite
#' @export
#' @examples
UpdateMongoWellCollection <- function(mongo_data_base, ResourcePlay=NA_real_, CPU_count=1, PurgeWellCollection=FALSE) {
  #library(mongolite)
  #library(jsonlite)
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
    if(FALSE) {
    UWIs <- mongodb$distinct(
      "UWI",
      toJSON( 
        list(
          ResourcePlay = ResourcePlay
        ),
        auto_unbox=TRUE
      )
    )
    }
    
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
  
  UpdateMongoWellCollection_sub <- function(i) { 
    mongodb <- mongo(
      collection = mongo_data_base$collection, 
      url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")
    )
    mongodb.well <- mongo(
      collection = "well", 
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
              DataCurrencyString = 1,
              Welltype = 1,
              MonthsOn = 1,
              ResourcePlay = 1,
              loc = 1,
              Operator = 1,
              Formation = 1,
              PeakQuarter = 1,
              PeakRate = 1,
              PeakCGR = 1,
              PeakGOR = 1,
              MajorProduct = 1
            )
          ),
          list(
            "$sort" = list(
              "DataCurrencyString" = -1
            )
          ),
          list(
            '$group' = list(
              "_id" = "$Product",
              UWI = list(
                "$first" ="$UWI"
              ),
              Welltype = list(
                "$first" ="$Welltype"
              ),
              MonthsOn = list(
                "$first" ="$MonthsOn"
              ),
              ResourcePlay = list(
                "$first" ="$ResourcePlay"
              ),
              loc = list(
                "$first" ="$loc"
              ),
              Operator = list(
                "$first" ="$Operator"
              ),
              Formation = list(
                "$first" ="$Formation"
              ),
              Product = list(
                "$first" ="$MajorProduct"
              ),
              PeakQuarter = list(
                "$first" ="$PeakQuarter"
              ),
              PeakRate = list(
                "$first" ="$PeakRate"
              ),
              PeakCGR = list(
                "$first" ="$PeakCGR"
              ),
              PeakGOR = list(
                "$first" ="$PeakGOR"
              )
            )
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
    
    
    #reformat buf so our update command will accept it
    buf2 <- as.list(buf)
    buf2$loc <- list(
      type = buf2$loc$type,
      coordinates = buf2$loc$coordinates[[1]]
    )
    buf2 <- list(
      "$set" = buf2
    )
    
    
    criteria <- list(
      UWI = i
    )
    
    #remove duplicate UWIs... might do later, if necessary
    #http://stackoverflow.com/questions/14184099/fastest-way-to-remove-duplicate-documents-in-mongodb
    
    #update the record in the well collection
    mongodb.well$update(
      query=toJSON(criteria,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
      update=toJSON(buf2,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
      upsert=TRUE,
      multiple=TRUE
    )
    rm(criteria)
    rm(buf,buf2)
    rm(mongodb, mongodb.well)
  }
  
  
  if(length(UWIs)>0) {
    sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
    sfExport("UpdateMongoWellCollection_sub", "mongo_data_base")
    sfLibrary(jsonlite)
    sfLibrary(mongolite)
    json_list <- sfClusterApplyLB(as.list(UWIs), function(i) {UpdateMongoWellCollection_sub(i)})
    #json_list <- lapply(as.list(UWIs[1:10]), function(i) {UpdateMongoWellCollection_sub(i)})
    sfStop()
    
    #Remove all UWIs in the well collection that aren't in the list we just pulled
    if(PurgeWellCollection) {
      buf <- list(
        ResourcePlay = ResourcePlay,
        UWI = list(
          "$nin" = UWIs 
        )
      )
      mongodb.well$remove(
        query=toJSON(buf,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
        multiple=TRUE
      )
      rm(buf)
    }
    
    return(sum(unlist(json_list)))
  } else {
    return(0)
  }
  
  rm(mongodb, mongodb.well, UWIs)
  gc()
}