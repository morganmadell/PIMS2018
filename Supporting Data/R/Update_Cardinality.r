#' Updates the "Cardinality" object of DeclineAnalysis array elements
#' 
#' \code{Update_Cardinality}
#'
#' @param mongo_data_base database location and authentication credentials
#' @param ResourcePlay Restrict updates to a specified resource play
#' @param CPU_count number of CPU cores to spool up to run update threads
#' @param ForceUWI Force re-evaluation of specified wells
#' @importFrom  mongolite jsonlite doSNOW snowfall
#' @export
#' @examples
Update_Cardinality <- function(data_base, mongo_data_base, ResourcePlay=NA_real_, CPU_count=1, ForceUpdate=FALSE) {
  #library(mongolite)
  #library(jsonlite)
  #library(doSNOW)
  #library(snowfall)
  OracleLimit <- 1000
  CardinalityDistance <- sqrt((1609.34^2)/pi)
  mongodb <- mongo(collection = mongo_data_base$collection, url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")  )
  mongodb.well <- mongo(collection = "well", url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")  )
  channel01 <- odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd)
  
  
  Cardinality_sub <- function(i, ForceUpdate=FALSE) {
    mongodb <- mongo(collection = mongo_data_base$collection, url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/", mongo_data_base$dbname,sep=""))
    mongodb.well <- mongo(collection = "well", url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")  )
    
    criteria <- list(
      UWI = onstream_data[i,1]
    )
    
    if(!ForceUpdate) {
      criteria <- c(criteria,
                    list(
                      Cardinality = list(
                        "$exists" = FALSE
                      )
                    )
      )
    }
    
    iter <- mongodb$iterate(toJSON(criteria,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"))
    update_count <- mongodb$count(toJSON(criteria,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"))
    
    # Iterate over individual records
    while(!is.null(doc <- iter$one())){
      
      criteria2 <-   list(
        ResourcePlay = ResourcePlay,
        loc = list(
          "$nearSphere" = list(
            "$geometry" = list(
              type = doc$loc$type, 
              coordinates = unlist(doc$loc$coordinates)
            ),
            spherical = TRUE,
            "$maxDistance" = CardinalityDistance
          )
        )
      )
      
      Offset_Wells <- mongodb.well$find(
        query=toJSON(criteria2,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
        fields=toJSON(list("UWI"),auto_unbox=TRUE,POSIXt="mongo",null="null",na="null")
        )
      
      onstream_data$X_ONPROD_DATE[onstream_data$X_UWI_DISPLAY==doc$UWI]
      Offset_Onstreams <- onstream_data[match(Offset_Wells$`_id`, onstream_data$X_UWI_DISPLAY),]
      Offset_Onstreams <- Offset_Onstreams[order(Offset_Onstreams$X_ONPROD_DATE),]
      Cardinality <- match(onstream_data$X_ONPROD_DATE[onstream_data$X_UWI_DISPLAY==doc$UWI], Offset_Onstreams$X_ONPROD_DATE)
      
      
      criteria3 <- list(
        UWI = doc$UWI,
        DataCurrencyString = doc$DataCurrencyString,
        Product = doc$Product
      )
      
      buf <- list(
        "$set" =
          list(
            "LastUpdated"=as.POSIXct(Sys.time(), tz="America/Denver", usetz=TRUE),
            "LastUpdatedString"=format(Sys.time(), tz="America/Denver", usetz=TRUE),
            "Cardinality" = Cardinality
          )
      )
      
      
      mongodb$update(
        query=toJSON(criteria3,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
        update=toJSON(buf,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
        upsert=TRUE,
        multiple=TRUE
      )
      #print(doc$DataCurrencyString)
    }
    
    rm(Offset_Wells,Offset_Onstreams,Cardinalty)
    rm(criteria, criteria2, criteria3)
    rm(buf, iter, doc)
    rm(mongodb,mongodb.well)
    gc()
    return(update_count)
  }
  
  
  #If no ResourcePlay was specified, find one with at least one well with production that is missing the Cardinality field
  if(is.na(ResourcePlay)) {
    ResourcePlay <- mongodb$find(
      toJSON(
        list(
          PeakRate = list(
            "$gt" = 0
          ),
          Cardinality = list(
            "$exists" = FALSE
          )
        ),
        auto_unbox=TRUE
      ),
      limit=1
    )$ResourcePlay
  }
  
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
  
  
  #pull in the onstream dates
  temp <- UWIs[1:min(length(UWIs),OracleLimit)]
  
  if(data_base$vendor =="PPDM") Well_Query <- "select WELL.X_UWI_DISPLAY, WELL.X_ONPROD_DATE from WELL where WELL.X_UWI_DISPLAY in ('"
  if(data_base$vendor =="HPDI") Well_Query <- "select HPDI_PDEN_DESC.ENTITY_ID, HPDI_PDEN_DESC.FIRST_PROD_DATE from HPDI_PDEN_DESC where HPDI_PDEN_DESC.ENTITY_ID in ('"
  
  onstream_query <- paste(Well_Query, paste(temp, collapse = "', '"), "')",sep="")
  onstream_data <- sqlQuery(channel01, onstream_query, stringsAsFactors=FALSE)  
  
  l=1
  while(length(UWIs)>OracleLimit*l) {
    temp <- UWIs[(1:min((length(UWIs)-OracleLimit*l),OracleLimit))+OracleLimit*l]
    onstream_query <- paste(Well_Query, paste(temp, collapse = "', '"), "')",sep="")
    onstream_data <- rbind(
      onstream_data,
      sqlQuery(channel01, onstream_query, stringsAsFactors=FALSE)
    )
    l <- l+1
  }
  rm(temp,l)
  
  
  if(data_base$vendor =="HPDI") {
    onstream_data <- renameColumns(onstream_data, from = c("ENTITY_ID", "FIRST_PROD_DATE"), to = c("X_UWI_DISPLAY", "X_ONPROD_DATE"))
  }
  
  if(data_base$vendor =="PPDM") {
  }
  
  
  
  
  
  if(length(onstream_data[,1])>0) {
    onstream_data <- onstream_data[with(onstream_data, order(X_UWI_DISPLAY)), ]
    row.names(onstream_data) <- 1:length(onstream_data[,1])
    sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
    sfExport("Cardinality_sub", "onstream_data", "mongo_data_base","ForceUpdate","ResourcePlay","CardinalityDistance")
    sfLibrary(jsonlite)
    sfLibrary(mongolite)
    json_list <- sfClusterApplyLB(as.list(1:length(onstream_data[,1])), function(i) {Cardinality_sub(i,ForceUpdate)})
    sfStop()
    return(sum(unlist(json_list)))
  } else {
    return(0)
  }
  
  rm(UWIs)
  rm(Well_Query)
  rm(onstream_query)
  rm(onstream_data)
  rm(mongodb)
  rm(channel01)
  gc()
}