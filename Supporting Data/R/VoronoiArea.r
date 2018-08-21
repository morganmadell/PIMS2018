#' Calculate the drainage area of each well in a dataframe using Voronoi tesselation
#' 
#' \code{VoronoiArea} requires an ff.df dataframe "well_data" with rownames 
#' X_UWI_DISPLAY,SURFACE_LATITUDE,SURFACE_LONGITUDE
#' if the the optional argument PLOT=TRUE, 
#' generates a plot showing the wells with their 2D production and the Voronoi tesselation used
#'
#' Returns a three column dataframe:
#'   X_UWI_DISPLAY     Well Name
#'   Area              Approximate drainage area of well (square m)
#'   Perimeter         Approximate perimeter of drainage area (m)
#'
#' @param well_data ffdf data frame to parse (that's right, it requires an ff imput!)
#' @param PLOT plot results to current device (default PLOT=FALSE)
#' @import ffbase
#' @importFrom deldir deldir
#' @importFrom deldir tile.list
#' @importFrom deldir tilePerim
#' @importFrom geoR nearloc
#' @importFrom sp SpatialPoints
#' @importFrom sp spTransform
#' @importFrom sp coordinates
#' @export
#' @examples
#' library(ffbase)
#' X_UWI_DISPLAY <- as.ff(as.factor(c("91/09-05-035-26W3/0","91/12-05-035-26W3/0","92/09-05-035-26W3/0","92/12-05-035-26W3/0")))
#' SURFACE_LATITUDE <- as.ff(c(51.97890,51.97898,51.97709,51.97700))
#' SURFACE_LONGITUDE <- as.ff(c(-109.6851,-109.6796,-109.6818,-109.6742))
#' data <- ffdf(X_UWI_DISPLAY,SURFACE_LATITUDE,SURFACE_LONGITUDE)
#' VoronoiArea(data)
#' rm(SURFACE_LATITUDE,SURFACE_LONGITUDE,X_UWI_DISPLAY,data)

VoronoiArea <- function(well_data, PLOT=FALSE) {
  
  ##################################################################################################
  # Define the coordinate system used by the BC Ministry of Energy, Mines, and Petroleum Resources #
  ##################################################################################################
  
  oldproj <- "+proj=longlat +datum=NAD27"       # Default datum
  origproj <- "+proj=longlat +datum=NAD83"       # Default datum
  googleproj <- "+proj=longlat +datum=WGS84"    # Google datum
  aea.proj <- "+proj=aea +lat_1=50.0 +lat_2=58.5 +lat_0=56.0 +lon_0=-126.0 +x_0=0 +y_0=0 +datum=NAD83 +units=m"  # Standard BC projection: "BC Albers"
  
  wells_points <- SpatialPoints(
    cbind(
      well_data$SURFACE_LONGITUDE[,][well_data$SURFACE_LATITUDE[,]>0],
      well_data$SURFACE_LATITUDE[,][well_data$SURFACE_LATITUDE[,]>0]
    ),
    proj4string = CRS(origproj)
  )
  #project data to a flat plane using units of metres and a local optimum bounding box
  local.proj <- paste("+proj=aea +lat_1=", bbox(wells_points)[2,1], " +lat_2=", bbox(wells_points)[2,2], " +lat_0=", mean(bbox(wells_points)[2,]), " +lon_0=", mean(bbox(wells_points)[1,]), " +x_0=0 +y_0=0 +datum=WGS84 +units=m",sep="")
  wells_points <- spTransform(wells_points, CRS(local.proj))
  x <- coordinates(wells_points)[,1]
  y <- coordinates(wells_points)[,2]
  
  #range_x = max(x)-min(x)
  #range_y = max(y)-min(y)
  offset = 1609.344  #one mile, in metres
  
  #tv <- deldir(x,y,list(ndx=2,ndy=2),c(0,10,0,10))
  #tv <- deldir(x,y,list(ndx=2,ndy=2),c(0,10,0,10),plot=TRUE,z=z,zdum=zdum)
  #tv <- deldir(x,y,list(ndx=2,ndy=2),plot=TRUE,z=z,zdum=zdum)
  #tv <- deldir(x,y,plot=TRUE)
  tv <- deldir(x,y,rw=c(min(x)-3*offset, max(x)+3*offset, min(y)-3*offset, max(y)+3*offset),plot=PLOT)
  
  #area is given by tv$summary$dir.area
  #current code gives area in square metres
  
  #for perimeters of tiles... the tile.list command is slow
  tl <- tile.list(tv)
  perim <- tilePerim(tl)
  
  #sometimes we have duplicate points (ie the /0 and/2 well events)
  #in such cases deldir only returns the areas for each unique datapoints
  #the following code compares number of locations
  #if they don't match we need to match our input locations (duplicates included) to our output locations (duplicates removed)
  if(length(x)==length(tv$summary$x)) {
    matches <- 1:length(tv$summary$x)
  } else {  
    matches <- nearloc(data.frame(x=x,y=y), data.frame(x=tv$summary$x,y=tv$summary$y), positions = TRUE)
  }
  
  #return(c(as.vector(tv$summary$dir.area),tilePerim(tl)))
  return(
    data.frame(
      X_UWI_DISPLAY = well_data$X_UWI_DISPLAY[well_data$SURFACE_LATITUDE[,]>0],
      Area = as.vector(tv$summary$dir.area)[matches],
      Perimeter = perim$perimeters[matches]
    )
  )
}
