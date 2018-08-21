#' Updates the "well" Collection for a Mongo database that follows the GLJ "base_decline" Schema.
#' 
#' \code{UpdateLocationsMongoWellCollection}
#'
#' @param mongo_data_base database location and authentication credentials
#' @param origproj CRS projection of original data, defaults to "+proj=longlat +datum=NAD27"
#' @param newproj CRS projection desired, defaults to "+proj=longlat +datum=WGS84"
#' @param CPU_count number of CPU cores to spool up to run update threads
#' @importFrom  mongolite jsonlite rgdal sp doSNOW snowfall
#' @export
#' @examples
UpdateLocationsMongoWellCollection <- function(mongo_data_base, origproj="+proj=longlat +datum=NAD27", newproj="+proj=longlat +datum=WGS84", CPU_count=1, limit=1000) {
  #library(mongolite)
  #library(jsonlite)
  #library(rgdal)
  #library(sp)
  #library(doSNOW)
  #library(snowfall)
  raw.proj <- origproj
  new.proj <- newproj
  
  UpdateLocationsMongoWellCollection_sub <- function(i) {
    
    criteria <- list(
      UWI=locations.proj$UWI[i]
    )
    buf <- list(
      UWI=locations.proj$UWI[i],
      "LastUpdated"=as.POSIXct(Sys.time(), tz="America/Denver", usetz=TRUE),
      "LastUpdatedString"=format(Sys.time(), tz="America/Denver", usetz=TRUE),
      crs=list(
        type = "name",
        properties = list(
          href = "http://spatialreference.org/ref/epsg/4326/proj4/",
          type = "proj4"
        )
      ),
      loc=list(
        type="Point",
        coordinates=as.numeric(
          coordinates(locations.proj[i,])
        )
      )
    )
    mongodb <- mongo(collection = mongo_data_base$collection, url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/", mongo_data_base$dbname,sep=""))
    try(
      mongodb$update(
        query=toJSON(criteria,auto_unbox=TRUE),
        update=paste("{\"$set\":",toJSON(buf,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),"}",sep=""),
        upsert=TRUE,
        multiple=TRUE
      )
      ,TRUE)
    rm(criteria)
    rm(buf)
    rm(mongodb)
  }
  
  mongodb <- mongo(
    collection = mongo_data_base$collection, 
    url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")
  )
  
  
  dbPull <- mongodb$aggregate(
    pipeline = toJSON(
      list(
        list(
          "$match" = list(
            "$or" = list(
              list(
                loc = list(
                  '$exists' = TRUE
                ),
                crs.type = list(
                  '$exists' = FALSE
                ),
                loc.coordinates = list(
                  '$ne' = 'null'
                )
              ),
              list(
                loc = list(
                  '$exists' = TRUE
                ),
                crs.type = "NAD27",
                loc.coordinates = list(
                  '$ne' = 'null'
                )
              )
            )
          )
        ),
        list(
          "$limit" = limit*2
        ),
        list(
          "$sort" = list(
            "MonthsOn" = -1
          )
        ),
        list(
          '$group' = list(
            "_id" = "$UWI",
            UWI = list(
              "$first" ="$UWI"
            ),
            loc = list(
              "$first" ="$loc"
            )
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
  
  
  if(length(dbPull$UWI)>1) {
    
    locations <- SpatialPointsDataFrame(
      data=data.frame("UWI"=dbPull$UWI),
      coords=matrix(unlist(dbPull$loc$coordinates),ncol=2,byrow=TRUE), 
      proj4string = CRS(raw.proj)
    )
    locations.proj <- spTransform(locations, CRS(new.proj))
    
    #lapply(1:length(locations$UWI), UpdateLocationsMongoWellCollection_sub)
    
    sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
    sfExport("UpdateLocationsMongoWellCollection_sub", "mongo_data_base", "locations", "locations.proj")
    sfLibrary(jsonlite)
    sfLibrary(mongolite)
    json_list <- sfSapply(1:length(locations$UWI), UpdateLocationsMongoWellCollection_sub)
    #json_list <- sfClusterApplyLB(1:length(locations$UWI), UpdateLocationsMongoWellCollection_sub)
    sfStop()
    
    rm(locations, locations.proj)
    return(length(dbPull$UWI))
  } else {
    return(NA)
  }
  
  rm(dbPull)
  rm(mongodb)
  rm(raw.proj,new.proj)
  gc()
}