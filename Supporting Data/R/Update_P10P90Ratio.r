#' Updates the "P10P90Ratio" object of DeclineAnalysis array elements
#' 
#' \code{Update_P10P90Ratio}
#'
#' @param mongo_data_base database location and authentication credentials
#' @param ResourcePlay Restrict updates to a specified resource play
#' @param CPU_count number of CPU cores to spool up to run update threads
#' @param ForceUWI Force re-evaluation of specified wells
#' @importFrom  mongolite jsonlite doSNOW snowfall MASS
#' @export
#' @examples
Update_P10P90Ratio <- function(mongo_data_base, ResourcePlay=NA_real_, CPU_count=1, ForceUpdate=FALSE) {
  #library(mongolite)
  #library(jsonlite)
  #library(doSNOW)
  #library(snowfall)
  #library(MASS)
  OneMile <- 1609.34
  mongodb <- mongo(collection = mongo_data_base$collection, url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")  )
  mongodb.well <- mongo(collection = "well", url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")  )

  
  P10P90Ratio_calculator <- function(data) {
    data <- na.omit(data)
    P10P90 <- NA_real_
    y <- ((1:length(data))-0.5)/length(data)
    x <- sort(data)
    if(length(data)<13) {
      if(length(data)>2) {
      y <- ((1:length(data))-0.5)/length(data)
      x <- sort(data)
      y_semilog <- y
      x_semilog <- log(x)
      bestfit <-lqs(y_semilog ~ x_semilog, method="lms")
      Prob <- exp((c(0.1,0.9)-coef(bestfit)[1])/coef(bestfit)[2])
      P10P90 <- try(Prob[2]/Prob[1])
      if(FALSE) {
        plot(x_semilog,y_semilog,xlim=log(c(100,10000)),xaxt="n", xlab="EUR (MMcf)", ylab = "Cumulative Probability", main=paste0("P10/P90 Ratio is ", as.character(round(P10P90, digits=2)), " for Wells Surrounding ", UWI$UWI[whichUWI]))
        axis(1,at=log(10^(2:4)),labels=formatC(c(10^(2:4)), big.mark = ",", format = "d"))
        axis.at <- 10^(2:4)
        axis(1, at = log(1:10 * rep(axis.at[-1] / 10, each = 10)),tcl = -0.5, labels = FALSE,tck=-0.01)
        abline(bestfit,col="red")
      }
      rm(x_semilog,y_semilog)
      rm(bestfit,Prob)
      }
    } else {
      y <- ((1:length(data))-0.5)/length(data)
      x <- sort(data)
      Prob <- approx(y,x,c(0.1,0.9))$y
      P10P90 <- try(Prob[2]/Prob[1])
      rm(Prob)
    }
    rm(x,y)
    return(P10P90)
  }
  
  P10P90Ratio_sub <- function(i, ForceUpdate=FALSE) {
    mongodb <- mongo(collection = mongo_data_base$collection, url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/", mongo_data_base$dbname,sep=""))
    mongodb.well <- mongo(collection = "well", url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")  )
    
    criteria <- list(
      UWI = i
    )
    
    if(!ForceUpdate) {
      criteria <- c(criteria,
                    list(
                      P10P90Ratio3mi = list(
                        "$exists" = FALSE
                      )
                    )
      )
    }
    
    iter <- mongodb$iterate(toJSON(criteria,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"))
    update_count <- mongodb$count(toJSON(criteria,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"))
    
    # Iterate over individual records
    while(!is.null(doc <- iter$one())){
      
      criteria2_3mi <-   list(
        ResourcePlay = ResourcePlay,
        loc = list(
          "$nearSphere" = list(
            "$geometry" = list(
              type = doc$loc$type, 
              coordinates = unlist(doc$loc$coordinates)
            ),
            spherical = TRUE,
            "$maxDistance" = 3*OneMile
          )
        )
      )
      
      criteria2_5mi <-   list(
        ResourcePlay = ResourcePlay,
        loc = list(
          "$nearSphere" = list(
            "$geometry" = list(
              type = doc$loc$type, 
              coordinates = unlist(doc$loc$coordinates)
            ),
            spherical = TRUE,
            "$maxDistance" = 5*OneMile
          )
        )
      )
      
      Offset_Wells_3mi <- mongodb.well$find(
        query=toJSON(criteria2_3mi,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
        fields=toJSON(list("UWI"),auto_unbox=TRUE,POSIXt="mongo",null="null",na="null")
        )[,1]
      
      Offset_Wells_5mi <- mongodb.well$find(
        query=toJSON(criteria2_5mi,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
        fields=toJSON(list("UWI"),auto_unbox=TRUE,POSIXt="mongo",null="null",na="null")
      )[,1]
      
      criteria_offsets_3mi <- list(
        UWI = list(
          "$in" = Offset_Wells_3mi
          ),
        DataCurrencyString = doc$DataCurrencyString,
        Product = doc$Product
      )
      
      criteria_offsets_5mi <- list(
        UWI = list(
          "$in" = Offset_Wells_5mi
        ),
        DataCurrencyString = doc$DataCurrencyString,
        Product = doc$Product
      )
      
      fields_offsets <- list(
        "_id"=0,
        "PeakRate"=1,
        "PeakQuarter"=1,
        "FracNorm.PeakRate"=1,
        "FracNorm.PeakQuarter"=1,
        "PropNorm.PeakRate"=1,
        "PropNorm.PeakQuarter"=1,
        "LengthNorm.PeakRate"=1,
        "LengthNorm.PeakQuarter"=1
      )
      
      Offset_Data_3mi <- mongodb$find(
        query=toJSON(criteria_offsets_3mi,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
        fields=toJSON(fields_offsets,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null")
      )
      
      Offset_Data_5mi <- mongodb$find(
        query=toJSON(criteria_offsets_5mi,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"),
        fields=toJSON(fields_offsets,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null")
      )
      
      
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
            "P10P90Ratio3mi" = list(
              PeakRate = P10P90Ratio_calculator(Offset_Data_3mi$PeakRate),
              PeakQuarter = P10P90Ratio_calculator(Offset_Data_3mi$PeakQuarter),
              FracNorm = list(
                PeakRate = P10P90Ratio_calculator(Offset_Data_3mi$FracNorm$PeakRate),
                PeakQuarter = P10P90Ratio_calculator(Offset_Data_3mi$FracNorm$PeakQuarter)
              ),
              PropNorm = list(
                PeakRate = P10P90Ratio_calculator(Offset_Data_3mi$PropNorm$PeakRate),
                PeakQuarter = P10P90Ratio_calculator(Offset_Data_3mi$PropNorm$PeakQuarter)
              ),
              LengthNorm = list(
                PeakRate = P10P90Ratio_calculator(Offset_Data_3mi$LengthNorm$PeakRate),
                PeakQuarter = P10P90Ratio_calculator(Offset_Data_3mi$LengthNorm$PeakQuarter)
              )
            ),
            "P10P90Ratio5mi" = list(
              PeakRate = P10P90Ratio_calculator(Offset_Data_5mi$PeakRate),
              PeakQuarter = P10P90Ratio_calculator(Offset_Data_5mi$PeakQuarter),
              FracNorm = list(
                PeakRate = P10P90Ratio_calculator(Offset_Data_5mi$FracNorm$PeakRate),
                PeakQuarter = P10P90Ratio_calculator(Offset_Data_5mi$FracNorm$PeakQuarter)
              ),
              PropNorm = list(
                PeakRate = P10P90Ratio_calculator(Offset_Data_5mi$PropNorm$PeakRate),
                PeakQuarter = P10P90Ratio_calculator(Offset_Data_5mi$PropNorm$PeakQuarter)
              ),
              LengthNorm = list(
                PeakRate = P10P90Ratio_calculator(Offset_Data_5mi$LengthNorm$PeakRate),
                PeakQuarter = P10P90Ratio_calculator(Offset_Data_5mi$LengthNorm$PeakQuarter)
              )
            )
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
    
    rm(Offset_Wells_3mi,Offset_Wells_5mi)
    rm(criteria, criteria2_3mi, criteria2_5mi, criteria3)
    rm(buf, iter, doc)
    rm(OneMile)
    rm(mongodb,mongodb.well)
    gc()
    return(update_count)
  }
  
  
  #If no ResourcePlay was specified, find one with at least one well with production that is missing the P10P90Ratio field
  if(is.na(ResourcePlay)) {
    ResourcePlay <- mongodb$find(
      toJSON(
        list(
          PeakRate = list(
            "$gt" = 0
          ),
          P10P90Ratio = list(
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
    #UWIs <- as.list(UWIs)
    gc()
  }
  
  if(length(UWIs)>0) {
    sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
    sfExport("P10P90Ratio_sub", "P10P90Ratio_calculator", "mongo_data_base","ForceUpdate","ResourcePlay","OneMile")
    sfLibrary(jsonlite)
    sfLibrary(mongolite)
    sfLibrary(MASS)
    json_list <- sfClusterApplyLB(as.list(UWIs), function(i) {P10P90Ratio_sub(i,ForceUpdate)})
    sfStop()
    return(sum(unlist(json_list)))
  } else {
    return(0)
  }
  
  rm(UWIs)
  rm(Well_Query)
  rm(mongodb)
  gc()
}