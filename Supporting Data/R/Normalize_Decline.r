#' Updates the "Forecast" object of DeclineAnalysis array elements that have "Method" equal to "ArpsDecline"
#' 
#' \code{Normalize_Decline}
#'
#' @param mongo_data_base database location and authentication credentials
#' @param CPU_count number of CPU cores to spool up to run update threads
#' @param ResourcePlay Restrict updates to a specified resource play
#' @param ForceUWI Force re-evaluation of specified wells
#' @importFrom  mongolite jsonlite doSNOW snowfall
#' @export
#' @examples
Normalize_Decline <- function(data_base, mongo_data_base, CPU_count=1, ResourcePlay=NA_real_, ForceUWI=NA_real_, limit=NA_real_) {
  #library(mongolite)
  #library(jsonlite)
  #library(doSNOW)
  #library(snowfall)
  OracleLimit <- 1000
  if(is.na(limit)) {limit <- OracleLimit}
  mongodb <- mongo(collection = mongo_data_base$collection, url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")  )
  channel01 <- odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd)
  
  longer <- function(x, y) {
    if(length(x)<length(y)) {
      return(y)
    } else {
      return(x)
    }
  }
  
  Normalize_sub <- function(i) {
    mongodb <- mongo(collection = mongo_data_base$collection, url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/", mongo_data_base$dbname,sep=""))
    
    criteria <- list(
      DeclineAnalysis = list(
        '$elemMatch' = list(
          Method = "ArpsDecline"
        )
      )
    )
    
    criteria2 <- c(criteria,
                   list(
                     UWI = as.character(frac_data[i,][[1]])
                   )
    )
    
    iter <- mongodb$iterate(toJSON(criteria2,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null"))
    
    # Iterate over individual records
    while(!is.null(doc <- iter$one())){
      
      
      DeclineAnalysisFracNorm <- lapply(
        1:length(doc$DeclineAnalysis), 
        function(j) {
          ee_names <- names(doc$DeclineAnalysis[[j]])
          test <- ee_names %in% c("Method", "EUR", "Rem", "Units", "AlgorithmReleaseDate")
          ee_out <- lapply(
            (1:length(ee_names))[test], 
            function(k) {
              if(ee_names[k] %in% c("Method", "AlgorithmReleaseDate")) {
                return(doc$DeclineAnalysis[[j]][[k]])
              } else {
                if(ee_names[k] %in% c("Units")) {
                  return(paste0(doc$DeclineAnalysis[[j]][[k]],"_per_stage"))
                } else {
                  if(ee_names[k] %in% c("EUR", "Rem")) {
                    return(longer(doc$DeclineAnalysis[[j]][[k]]/as.numeric(frac_data[i,][2]), NA_real_))
                  }
                }
              }
            }
          )
          names(ee_out) <- ee_names[test]
          rm(ee_names, test)
          ee_out
        }
      )
      
      DeclineAnalysisPropNorm <- lapply(
        1:length(doc$DeclineAnalysis), 
        function(j) {
          ee_names <- names(doc$DeclineAnalysis[[j]])
          test <- ee_names %in% c("Method", "EUR", "Rem", "Units", "AlgorithmReleaseDate")
          ee_out <- lapply(
            (1:length(ee_names))[test], 
            function(k) {
              if(ee_names[k] %in% c("Method", "AlgorithmReleaseDate")) {
                return(doc$DeclineAnalysis[[j]][[k]])
              } else {
                if(ee_names[k] %in% c("Units")) {
                  return(paste0(doc$DeclineAnalysis[[j]][[k]],"_per_tonne"))
                } else {
                  if(ee_names[k] %in% c("EUR", "Rem")) {
                    return(longer(doc$DeclineAnalysis[[j]][[k]]/as.numeric(frac_data[i,][3]), NA_real_))
                  }
                }
              }
            }
          )
          names(ee_out) <- ee_names[test]
          rm(ee_names, test)
          ee_out
        }
      )
      
      DeclineAnalysisLengthNorm <- lapply(
        1:length(doc$DeclineAnalysis), 
        function(j) {
          ee_names <- names(doc$DeclineAnalysis[[j]])
          test <- ee_names %in% c("Method", "EUR", "Rem", "Units", "AlgorithmReleaseDate")
          ee_out <- lapply(
            (1:length(ee_names))[test], 
            function(k) {
              if(ee_names[k] %in% c("Method", "AlgorithmReleaseDate")) {
                return(doc$DeclineAnalysis[[j]][[k]])
              } else {
                if(ee_names[k] %in% c("Units")) {
                  return(paste0(doc$DeclineAnalysis[[j]][[k]],"_per_meter"))
                } else {
                  if(ee_names[k] %in% c("EUR", "Rem")) {
                    return(longer(doc$DeclineAnalysis[[j]][[k]]/as.numeric(frac_data[i,][4]), NA_real_))
                  }
                }
              }
            }
          )
          names(ee_out) <- ee_names[test]
          rm(ee_names, test)
          ee_out
        }
      )
      
      
      
      criteria3 <- c(criteria,
                     list(
                       UWI = doc$UWI,
                       DataCurrencyString = doc$DataCurrencyString,
                       Product = doc$Product
                     )
      )
      
      buf <- list(
        "$set" =
          list(
            "LastUpdated"=as.POSIXct(Sys.time(), tz="America/Denver", usetz=TRUE),
            "LastUpdatedString"=format(Sys.time(), tz="America/Denver", usetz=TRUE),
            "FracNorm" = list(
              "PeakRate" = longer(doc$PeakRate/as.numeric(frac_data[i,][2]), NA_real_),
              "PeakQuarter" = longer(doc$PeakQuarter/as.numeric(frac_data[i,][2]), NA_real_),
              "DeclineAnalysis" = DeclineAnalysisFracNorm
            ),
            "PropNorm" = list(
              "PeakRate" = longer(doc$PeakRate/as.numeric(frac_data[i,][3]), NA_real_),
              "PeakQuarter" = longer(doc$PeakQuarter/as.numeric(frac_data[i,][3]), NA_real_),
              "DeclineAnalysis" = DeclineAnalysisPropNorm
            ),
            "LengthNorm" = list(
              "PeakRate" = longer(doc$PeakRate/as.numeric(frac_data[i,][4]), NA_real_),
              "PeakQuarter" = longer(doc$PeakQuarter/as.numeric(frac_data[i,][4]), NA_real_),
              "DeclineAnalysis" = DeclineAnalysisLengthNorm
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
    
    rm(criteria, criteria2, criteria3)
    rm(DeclineAnalysisFracNorm, DeclineAnalysisPropNorm, DeclineAnalysisLengthNorm)
    rm(buf, iter, doc)
    rm(mongodb)
    gc()
    return(c(i, as.character(frac_data[i,][[1]]), as.numeric(frac_data[i,][2])))
  }
  
  #Get a list of wells in the specified ResourcePlay
  if(!is.na(ResourcePlay)) {
    UWIs <- mongodb$find(
      query = toJSON(
        list(
          ResourcePlay = ResourcePlay,
          FracNorm = list(
            "$exists" = FALSE
          )
        ),
        auto_unbox=TRUE
      ),
      fields = toJSON(
        list(
          UWI = 1
        ),
        auto_unbox=TRUE
      ),
      limit=limit
    )
    UWIs <- UWIs$UWI
    UWIs <- UWIs[!duplicated(UWIs)]
  }
  
  #Add in the wells specified for forced evaluation
  if(!is.na(ForceUWI)) {
    if(exists(UWIs)) {
      UWIs <- c(UWIs,ForceUWI)
    } else {
      UWIs <- ForceUWI
    }
  }
  
  #If neither a list of wells nor a ResourcePlay was explicitly specified, find some wells to evaluate
  if(is.na(ResourcePlay) && is.na(ForceUWI)) {
    UWIs <- mongodb$aggregate(
      pipeline = toJSON( 
        list(
          
          list(
            "$match" = list(
              PeakRate = list(
                "$gt" = 0
              ),
              FracNorm = list(
                "$exists" = FALSE
              ),
              DeclineAnalysis = list(
                '$elemMatch' = list(
                  Method = "ArpsDecline"
                )
              )
            )
          ),
          list(
            '$limit' = 10*limit
          ),
          list(
            '$group' = list(
              "_id" = "$UWI",
              UWI = list(
                "$first" ="$UWI"
              )
            )
          ),
          list(
            '$limit' = limit
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
    
  }
  
  temp <- UWIs[1:min(length(UWIs),OracleLimit)]
  frac_query <- paste("select FRAC_MASTER.UWI, FRAC_MASTER.STAGES_ACTUAL_#, FRAC_MASTER.TOTAL_PROPPANT_PLACED_T, FRAC_MASTER.COMPLETED_LENGTH_M from FRAC_MASTER where FRAC_MASTER.UWI in ('",
                      paste(temp, collapse = "', '"), "')",sep="")
  frac_data <- sqlQuery(channel01, frac_query, stringsAsFactors=FALSE)  
  
  l=1
  while(length(UWIs)>OracleLimit*l) {
    temp <- UWIs[(1:min((length(UWIs)-OracleLimit*l),OracleLimit))+OracleLimit*l]
    frac_query <- paste("select FRAC_MASTER.UWI, FRAC_MASTER.STAGES_ACTUAL_#, FRAC_MASTER.TOTAL_PROPPANT_PLACED_T, FRAC_MASTER.COMPLETED_LENGTH_M from FRAC_MASTER where FRAC_MASTER.UWI in ('",
                        paste(temp, collapse = "', '"), "')",sep="")
    frac_data<-rbind(
      frac_data,
      sqlQuery(channel01, frac_query, stringsAsFactors=FALSE)
      )
    l <- l+1
  }
  rm(temp)

  
  if(length(frac_data[,1])>0) {
    frac_data <- frac_data[with(frac_data, order(UWI)), ]
    row.names(frac_data) <- 1:length(frac_data[,1])
    sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
    sfExport("Normalize_sub", "longer", "frac_data", "mongo_data_base")
    sfLibrary(jsonlite)
    sfLibrary(mongolite)
    json_list <- sfClusterApplyLB(as.list(1:length(frac_data[,1])), function(i) {Normalize_sub(i)})
    #json_list <- sfSapply(1:length(frac_data[,1]), function(i) Normalize_sub(i))
    sfStop()
    #json_list <- lapply(as.list(1:length(frac_data[,1])), function(i) Normalize_sub(i))
    #for(i in 1:length(frac_data[,1])) Normalize_sub(i)
    return(length(frac_data[,1]))
  } else {
    return(0)
  }
  
  rm(UWIs)
  rm(frac_query)
  rm(frac_data)
  rm(mongodb)
  rm(channel01)
  gc()
}