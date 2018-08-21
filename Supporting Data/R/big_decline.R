#required in main big_decline.R file
library(doSNOW)
library(ffbase)
library(plyr)
library(RColorBrewer)
library(ETLUtils)
library(RODBC)
library(doBy)
library(zoo)
library(snowfall)
library(mongolite)
library(R.utils)
library(deldir)
library(geoR)
library(sp)
library(jsonlite)
library(lubridate)
library(aRpsDCA)

#required for Gas_Reserves.r and Oil_Reserves.r
if(FALSE){
  library(LambertW)
  #library(MASS)
}

#required for Seg.Well.Anal.Exp.r, Seg.Well.Anal.Harm.r and Seg.Well.Anal.Watten.r
if(FALSE){
  library(mgcv)
}


#required for segmented.lm.break.r
if(FALSE){
  library(segmented)
}

#required for Peak_Quarter
if(FALSE){
  library(zoo)
}


if(FALSE){
  library(classInt)
  library(colorspace)
  library(cwhmisc)
  library(nloptr)
  library(quantreg)
  library(roxygen2)
  library(strucchange)
  library(TeachingDemos)
  library(timeSeries)
}


if(FALSE){
  library(rgdal)
}





#os <- "Linux"
os <- "Windows"

#origproj <- "+proj=longlat +datum=NAD83" # Default datum
origproj <- "+proj=longlat +datum=NAD27"
newproj <- "+proj=longlat +datum=WGS84"
googleproj <- "+proj=longlat +datum=WGS84"    # Google datum
aea.proj <- "+proj=aea +lat_1=50.0 +lat_2=58.5 +lat_0=56.0 +lon_0=-126.0 +x_0=0 +y_0=0 +datum=NAD83 +units=m"  # Standard BC projection: "BC Albers"
data_base <- data.frame(vendor="Empty", dsn="Empty", uid="Empty",pwd="Empty")
mongo_data_base <- data.frame(host="Empty",  rsname="Empty", dbname="Empty", timeout=NA_integer_, user="Empty", pwd="Empty", collection="Empty")
#data_base$vendor <- "HPDI"
data_base$vendor <- "PPDM"
max_wells_process <- 10000  # maximum number of wells to analyse in one pass... limitted by RAM
max_wells_dbquery <- 1000	# maximum number of wells you send to a single database query... GLJ's Oracle server limits this to 1000
#products <- c("Gas","Oil")  # choose from Gas, Oil (just these two for now... water and cond can be added as necessary)
products <- c("Gas")  # choose from Gas, Oil (just these two for now... water and cond can be added as necessary)
#products <- c("Oil")  # choose from Gas, Oil (just these two for now... water and cond can be added as necessary)
save_yields <- FALSE
calculate_segmented <- FALSE
calculate_stan <- FALSE
calculate_areas <- TRUE
calculate_all_dates <- FALSE
calculate_many_dates <- TRUE
plot_segmented <- FALSE
save_seg_tables <- TRUE
json_save <- TRUE
mongodb_save <- TRUE
build_documentation <- FALSE
#ResourcePlay <- "Shallow Gas"
ResourcePlay <- "Montney"
Welltype <- "Undefined"
#Welltype <- "Vertical"
#Welltype <- "Horizontal"
iters <- 1000


if(os=="Linux") {
  #export RSTUDIO_WHICH_R=/opt/r/R-3.1.1/bin/R
  CPU_count <- 4
  workdir <- "~/Downloads/BigDeclineCode/"
  options(fftempdir="/mike/fftemp/")
} else {
  CPU_count <- 6
  workdir <- "C:/temp/BigDeclineCode/"
  options(fftempdir=paste(workdir, "fftemp", sep=""))
}
setwd(workdir)

sapply(list.files(pattern="[.]r$", path=paste(workdir, "R", sep=""), full.names=TRUE), source)

if(build_documentation) {
  write.dcf(
    list(Package = "BigDeclineCode",
         Title = "Big Decline", 
         Description = "Tools to calculate decline parameters for bulk well lists.", 
         Version = "0.0", 
         License = "For my eyes only", 
         Author = "Michael Morgan <mmorgan@gljpc.com>", 
         Maintainer = "Michael Morgan <mmorgan@gljpc.com>",
         Imports = "aRpsDCA, deldir, doBy, doSNOW, ETLUtils, ffbase, geoR, jsonlite, LambertW, lubridate, MASS, plyr, quantreg, RColorBrewer, rgdal, rstan, mongolite, RODBC, roxygen2, R.utils, segmented, snowfall, sp, strucchange, TeachingDemos, timeSeries, zoo",
         Suggests = "knitr",
         VignetteBuilder = "knitr"
    ), 
    file = file.path(workdir, "DESCRIPTION")
  )
  roxygen2::roxygenise()
  
  #devtools::build_vignettes()
  #devtools::document()
  #devtools::build(vignettes=TRUE, manual=TRUE)
  #devtools::build(binary=TRUE, vignettes=TRUE, manual=TRUE)
  #the following creates a sample vignette
  #devtools::use_vignette("my-vignette")
}


if(data_base$vendor =="PPDM") well_query <- "select WELL.X_UWI_DISPLAY, WELL.UWI, WELL.SURFACE_LATITUDE, WELL.SURFACE_LONGITUDE, WELL.X_TD_TVD, WELL.DRILL_TD, WELL.X_ONPROD_DATE, WELL.RIG_RELEASE_DATE, WELL.PROFILE_TYPE, GLJ_PDEN_SUMMARY.PSUM_POOL_NAME, GLJ_PDEN_SUMMARY.PSUM_OPERATOR_NAME from WELL, GLJ_PDEN_SUMMARY where GLJ_PDEN_SUMMARY.PSUM_UWI=WELL.UWI and WELL.X_UWI_DISPLAY in ('"
if(data_base$vendor =="PPDM") prod_query <- "select WELL.X_UWI_DISPLAY, WELL.X_TD_TVD, PDEN_PRODUCTION_MONTH.PROD_DATE, PDEN_PRODUCTION_MONTH.GAS, PDEN_PRODUCTION_MONTH.WATER, PDEN_PRODUCTION_MONTH.OIL_BT, PDEN_PRODUCTION_MONTH.COND, PDEN_PRODUCTION_MONTH.CUM_GAS, PDEN_PRODUCTION_MONTH.CUM_OIL_BT, PDEN_PRODUCTION_MONTH.CUM_WATER, PDEN_PRODUCTION_MONTH.CUM_COND, PDEN_PRODUCTION_MONTH.TOTAL_FLUID, PDEN_PRODUCTION_MONTH.GAS_CAL_DAY, PDEN_PRODUCTION_MONTH.OIL_CAL_DAY, PDEN_PRODUCTION_MONTH.WATER_CAL_DAY, PDEN_PRODUCTION_MONTH.COND_CAL_DAY, PDEN_PRODUCTION_MONTH.TOTAL_FLUID_CAL_DAY, PDEN_PRODUCTION_MONTH.GAS_ACT_DAY, PDEN_PRODUCTION_MONTH.OIL_ACT_DAY, PDEN_PRODUCTION_MONTH.WATER_ACT_DAY, PDEN_PRODUCTION_MONTH.COND_ACT_DAY, PDEN_PRODUCTION_MONTH.TOTAL_FLUID_ACT_DAY from WELL, PDEN_PRODUCTION_MONTH where PDEN_PRODUCTION_MONTH.PDEN_ID=WELL.UWI and WELL.X_UWI_DISPLAY in ('"

if(data_base$vendor =="HPDI") well_query <- "select HPDI_PDEN_DESC.ENTITY_ID, HPDI_PDEN_DESC.API_NO, HPDI_PDEN_DESC.LATITUDE, HPDI_PDEN_DESC.LONGITUDE, HPDI_PDEN_DESC.LOWER_PERF, HPDI_PDEN_DESC.TOTAL_DEPTH, HPDI_PDEN_DESC.FIRST_PROD_DATE, HPDI_PDEN_DESC.COMP_DATE, HPDI_PDEN_DESC.RESERVOIR, HPDI_PDEN_DESC.CURR_OPER_NAME from HPDI_PDEN_DESC where HPDI_PDEN_DESC.ENTITY_ID in ('"
if(data_base$vendor =="HPDI") prod_query <- "select HPDI_PDEN_DESC.ENTITY_ID, HPDI_PDEN_DESC.TOTAL_DEPTH, HPDI_PDEN_PROD.PROD_DATE, HPDI_PDEN_PROD.GAS, HPDI_PDEN_PROD.WTR, HPDI_PDEN_PROD.LIQ from HPDI_PDEN_DESC, HPDI_PDEN_PROD where HPDI_PDEN_PROD.ENTITY_ID=HPDI_PDEN_DESC.ENTITY_ID and HPDI_PDEN_DESC.ENTITY_ID in ('"

#odbcConnect(dsn="PostgreSQL30",uid="monolith", pwd="ILaK|LiDL-Wj7=4s",case="postgresql")
#odbcConnect(dsn="PostgreSQL35W",uid="monolith", pwd="ILaK|LiDL-Wj7=4s",case="postgresql")

#odbcConnect(dsn="pgprod",uid="monolith", pwd="ILaK|LiDL-Wj7=4s",case="postgresql")
#odbcConnect(dsn="pg-prod",uid="monolith", pwd="ILaK|LiDL-Wj7=4s",case="postgresql")

if(FALSE) {
  odbcDriverConnect(connection = "", case, believeNRows = TRUE,
                    colQuote, tabQuote = colQuote,
                    interpretDot = TRUE, DBMSencoding = "",
                    rows_at_time = 100, readOnlyOptimize = FALSE)
  
  odbcDriverConnect(paste(dsn="pgprod",uid="monolith", pwd="ILaK|LiDL-Wj7=4s",sep=";"))
  odbcDriverConnect('driver = {postgresql}; user=monolith; password=ILaK|LiDL-Wj7=4s; host=pg-prod; port=5432; dbname=pgprod; trusted_connection = true')
  
  connection = "postgresql"
  
  
  library(DBI)
  con <- dbConnect(RPostgres::Postgres(),
                   dbname = 'pgprod', 
                   host = 'pg-prod', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                   user = 'monolith',
                   password = 'ILaK|LiDL-Wj7=4s')
}


#########################
# Load in the Well List #
#########################

if(ResourcePlay== "Bakken" && Welltype == "Horizontal" ) {
  well_list <- as.factor(rbind(
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN where PDEN.PDEN_ID=WELL.UWI and WELL.PROVINCE_STATE in ('SK', 'MB') and WELL.PROFILE_TYPE='H' and PDEN.STRAT_UNIT_ID in ('BAKKEN', 'LBAKKEN', 'MBAKKEN', 'UBAKKEN')")
   )$X_UWI_DISPLAY)
  well_list <- unique(well_list)
  products <- c("Oil") 
}
if(ResourcePlay== "Cardium" && Welltype == "Horizontal" ) {
  well_list <- as.factor(rbind(
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN, LEGAL_DLS_LOC where PDEN.PDEN_ID=WELL.UWI and LEGAL_DLS_LOC.UWI=WELL.UWI and WELL.PROVINCE_STATE='AB' and WELL.PROFILE_TYPE='H' and WELL.X_ONPROD_DATE > '2008-12-31' and LEGAL_DLS_LOC.DLS_MERIDIAN in (5,6) and LEGAL_DLS_LOC.DLS_TOWNSHIP between 25 and 75 and PDEN.STRAT_UNIT_ID in ('CARDSS', 'CARD')")
  )$X_UWI_DISPLAY)
  well_list <- unique(well_list)
  products <- c("Gas","Oil") 
} 
if(ResourcePlay== "Horn River" && Welltype == "Horizontal" ) {
  well_list <- as.factor(rbind(
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN, LEGAL_NTS_LOC where PDEN.PDEN_ID=WELL.UWI and LEGAL_NTS_LOC.UWI=WELL.UWI and WELL.PROVINCE_STATE in ('BC') and WELL.PROFILE_TYPE='H' and LEGAL_NTS_LOC.BLOCK='A' and LEGAL_NTS_LOC.PRIMARY_QUADRANGLE=94 and LEGAL_NTS_LOC.LETTER_QUADRANGLE='O' and LEGAL_NTS_LOC.SIXTEENTH in (1, 2, 3, 6, 7, 8, 9, 10, 15, 16) and PDEN.STRAT_UNIT_ID in ('EVIE', 'KLUA', 'MUSKWA', 'OTTERPK')"),
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN, LEGAL_NTS_LOC where PDEN.PDEN_ID=WELL.UWI and LEGAL_NTS_LOC.UWI=WELL.UWI and WELL.PROVINCE_STATE in ('BC') and WELL.PROFILE_TYPE='H' and LEGAL_NTS_LOC.BLOCK='B' and LEGAL_NTS_LOC.PRIMARY_QUADRANGLE=94 and LEGAL_NTS_LOC.LETTER_QUADRANGLE='O' and LEGAL_NTS_LOC.SIXTEENTH in (2, 3, 6, 7, 8, 9, 10, 15, 16) and PDEN.STRAT_UNIT_ID in ('EVIE', 'KLUA', 'MUSKWA', 'OTTERPK')"),
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN, LEGAL_NTS_LOC where PDEN.PDEN_ID=WELL.UWI and LEGAL_NTS_LOC.UWI=WELL.UWI and WELL.PROVINCE_STATE in ('BC') and WELL.PROFILE_TYPE='H' and LEGAL_NTS_LOC.BLOCK='C' and LEGAL_NTS_LOC.PRIMARY_QUADRANGLE=94 and LEGAL_NTS_LOC.LETTER_QUADRANGLE='O' and LEGAL_NTS_LOC.SIXTEENTH in (2, 6, 8, 9, 10, 15, 16) and PDEN.STRAT_UNIT_ID in ('EVIE', 'KLUA', 'MUSKWA', 'OTTERPK')"),
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN, LEGAL_NTS_LOC where PDEN.PDEN_ID=WELL.UWI and LEGAL_NTS_LOC.UWI=WELL.UWI and WELL.PROVINCE_STATE in ('BC') and WELL.PROFILE_TYPE='H' and LEGAL_NTS_LOC.BLOCK='A' and LEGAL_NTS_LOC.PRIMARY_QUADRANGLE=94 and LEGAL_NTS_LOC.LETTER_QUADRANGLE='O' and LEGAL_NTS_LOC.SIXTEENTH in (1, 2, 3, 6, 7, 8, 9, 10, 15, 16) and PDEN.STRAT_UNIT_ID in ('EVIE', 'KLUA', 'MUSKWA', 'OTTERPK')"),
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN, LEGAL_NTS_LOC where PDEN.PDEN_ID=WELL.UWI and LEGAL_NTS_LOC.UWI=WELL.UWI and WELL.PROVINCE_STATE in ('BC') and WELL.PROFILE_TYPE='H' and LEGAL_NTS_LOC.BLOCK='D' and LEGAL_NTS_LOC.PRIMARY_QUADRANGLE=94 and LEGAL_NTS_LOC.LETTER_QUADRANGLE='O' and LEGAL_NTS_LOC.SIXTEENTH in (1, 3, 6, 7, 8, 9, 10, 15, 16) and PDEN.STRAT_UNIT_ID in ('EVIE', 'KLUA', 'MUSKWA', 'OTTERPK')"),
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN, LEGAL_NTS_LOC where PDEN.PDEN_ID=WELL.UWI and LEGAL_NTS_LOC.UWI=WELL.UWI and WELL.PROVINCE_STATE in ('BC') and WELL.PROFILE_TYPE='H' and LEGAL_NTS_LOC.BLOCK='E' and LEGAL_NTS_LOC.PRIMARY_QUADRANGLE=94 and LEGAL_NTS_LOC.LETTER_QUADRANGLE='O' and LEGAL_NTS_LOC.SIXTEENTH in (2, 6, 7, 8, 10, 15, 16) and PDEN.STRAT_UNIT_ID in ('EVIE', 'KLUA', 'MUSKWA', 'OTTERPK')"),
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN, LEGAL_NTS_LOC where PDEN.PDEN_ID=WELL.UWI and LEGAL_NTS_LOC.UWI=WELL.UWI and WELL.PROVINCE_STATE in ('BC') and WELL.PROFILE_TYPE='H' and LEGAL_NTS_LOC.BLOCK='F' and LEGAL_NTS_LOC.PRIMARY_QUADRANGLE=94 and LEGAL_NTS_LOC.LETTER_QUADRANGLE='O' and LEGAL_NTS_LOC.SIXTEENTH in (1, 2, 3, 6, 7, 8, 9, 10, 15, 16) and PDEN.STRAT_UNIT_ID in ('EVIE', 'KLUA', 'MUSKWA', 'OTTERPK')"),
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN, LEGAL_NTS_LOC where PDEN.PDEN_ID=WELL.UWI and LEGAL_NTS_LOC.UWI=WELL.UWI and WELL.PROVINCE_STATE in ('BC') and WELL.PROFILE_TYPE='H' and LEGAL_NTS_LOC.BLOCK='G' and LEGAL_NTS_LOC.PRIMARY_QUADRANGLE=94 and LEGAL_NTS_LOC.LETTER_QUADRANGLE='O' and LEGAL_NTS_LOC.SIXTEENTH in (1, 2, 3, 6, 7, 8, 9, 10, 15, 16) and PDEN.STRAT_UNIT_ID in ('EVIE', 'KLUA', 'MUSKWA', 'OTTERPK')"),
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN, LEGAL_NTS_LOC where PDEN.PDEN_ID=WELL.UWI and LEGAL_NTS_LOC.UWI=WELL.UWI and WELL.PROVINCE_STATE in ('BC') and WELL.PROFILE_TYPE='H' and LEGAL_NTS_LOC.BLOCK='H' and LEGAL_NTS_LOC.PRIMARY_QUADRANGLE=94 and LEGAL_NTS_LOC.LETTER_QUADRANGLE='O' and LEGAL_NTS_LOC.SIXTEENTH in (2, 3, 6, 7, 8, 9, 10, 15, 16) and PDEN.STRAT_UNIT_ID in ('EVIE', 'KLUA', 'MUSKWA', 'OTTERPK')"),
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN, LEGAL_NTS_LOC where PDEN.PDEN_ID=WELL.UWI and LEGAL_NTS_LOC.UWI=WELL.UWI and WELL.PROVINCE_STATE in ('BC') and WELL.PROFILE_TYPE='H' and LEGAL_NTS_LOC.BLOCK='I' and LEGAL_NTS_LOC.PRIMARY_QUADRANGLE=94 and LEGAL_NTS_LOC.LETTER_QUADRANGLE='O' and LEGAL_NTS_LOC.SIXTEENTH in (1, 2, 6, 7, 8, 9, 16) and PDEN.STRAT_UNIT_ID in ('EVIE', 'KLUA', 'MUSKWA', 'OTTERPK')"),
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN, LEGAL_NTS_LOC where PDEN.PDEN_ID=WELL.UWI and LEGAL_NTS_LOC.UWI=WELL.UWI and WELL.PROVINCE_STATE in ('BC') and WELL.PROFILE_TYPE='H' and LEGAL_NTS_LOC.BLOCK='J' and LEGAL_NTS_LOC.PRIMARY_QUADRANGLE=94 and LEGAL_NTS_LOC.LETTER_QUADRANGLE='O' and LEGAL_NTS_LOC.SIXTEENTH in (1, 2, 6, 7, 8, 9, 10, 15) and PDEN.STRAT_UNIT_ID in ('EVIE', 'KLUA', 'MUSKWA', 'OTTERPK')"),
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN, LEGAL_NTS_LOC where PDEN.PDEN_ID=WELL.UWI and LEGAL_NTS_LOC.UWI=WELL.UWI and WELL.PROVINCE_STATE in ('BC') and WELL.PROFILE_TYPE='H' and LEGAL_NTS_LOC.BLOCK='K' and LEGAL_NTS_LOC.PRIMARY_QUADRANGLE=94 and LEGAL_NTS_LOC.LETTER_QUADRANGLE='O' and LEGAL_NTS_LOC.SIXTEENTH in (1, 2, 3, 6, 7, 8, 9, 10, 15, 16) and PDEN.STRAT_UNIT_ID in ('EVIE', 'KLUA', 'MUSKWA', 'OTTERPK')"),
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN, LEGAL_NTS_LOC where PDEN.PDEN_ID=WELL.UWI and LEGAL_NTS_LOC.UWI=WELL.UWI and WELL.PROVINCE_STATE in ('BC') and WELL.PROFILE_TYPE='H' and LEGAL_NTS_LOC.BLOCK='L' and LEGAL_NTS_LOC.PRIMARY_QUADRANGLE=94 and LEGAL_NTS_LOC.LETTER_QUADRANGLE='O' and LEGAL_NTS_LOC.SIXTEENTH in (1, 2, 7, 8, 9, 10, 15) and PDEN.STRAT_UNIT_ID in ('EVIE', 'KLUA', 'MUSKWA', 'OTTERPK')")
  )$X_UWI_DISPLAY)
  well_list <- unique(well_list)
  #length(well_list)
  #sqlFetch(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "LEGAL_NTS_LOC", max=25)
  #sqlTables(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), tableName = "L%")
  #temp <- sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select PDEN.STRAT_UNIT_ID from WELL, PDEN where PDEN.PDEN_ID=WELL.UWI and WELL.PROVINCE_STATE in ('BC') and WELL.PROFILE_TYPE='H'")
  #temp <- sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.TD_STRAT_UNIT_ID from WELL, PDEN where PDEN.PDEN_ID=WELL.UWI and WELL.PROVINCE_STATE in ('AB') and WELL.X_ONPROD_DATE > '2007-12-31' and WELL.PROFILE_TYPE='H'")
  
  #sqlFetch(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "FRAC_MASTER", max=25)
  #frac_query <- paste("select FRAC_MASTER.UWI, FRAC_MASTER.RESOURCE_PLAY, FRAC_MASTER.RESOURCE_PLAY_GROUP from FRAC_MASTER",sep="")
  #frac_data <- sqlQuery(channel01, frac_query, stringsAsFactors=FALSE)  
  #write.csv(frac_data, file = "frac_data.csv") 
  
  #frac_data <- read.odbc.ffdf(query="select * from FRAC_MASTER", odbcConnect.args=list(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd),VERBOSE=TRUE)
  #write.csv.ffdf(frac_data, file="frac_data.csv")
  
  
  products <- c("Gas") 
} 
if(ResourcePlay== "LAmaranth" && Welltype == "Horizontal" ) {
  well_list <- as.factor(rbind(
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN, LEGAL_DLS_LOC where PDEN.PDEN_ID=WELL.UWI and LEGAL_DLS_LOC.UWI=WELL.UWI and WELL.PROVINCE_STATE in ('SK', 'MB') and WELL.PROFILE_TYPE='H' and LEGAL_DLS_LOC.DLS_TOWNSHIP between 1 and 3 and PDEN.PRIMARY_PRODUCT = 'CRUDE-OIL' and PDEN.STRAT_UNIT_ID in ('AMRANTH', 'LAMRANTH', 'WATROS', 'WATROSRED')")
  )$X_UWI_DISPLAY)
  well_list <- unique(well_list)
  products <- c("Oil") 
} 
if(ResourcePlay== "Slave Point" && Welltype == "Horizontal" ) {
  well_list <- as.factor(rbind(
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN, LEGAL_DLS_LOC where PDEN.PDEN_ID=WELL.UWI and LEGAL_DLS_LOC.UWI=WELL.UWI and WELL.PROVINCE_STATE in ('AB') and WELL.PROFILE_TYPE='H' and LEGAL_DLS_LOC.DLS_MERIDIAN in (5) and LEGAL_DLS_LOC.DLS_RANGE not in (3, 4, 5, 22, 26) and LEGAL_DLS_LOC.DLS_TOWNSHIP not in (38, 40, 41, 43, 46, 47, 48, 49, 52, 55, 56, 91, 92, 93, 94, 95, 96, 97, 104, 105, 106, 107, 112, 113, 115, 122) and PDEN.STRAT_UNIT_ID in ('BHLK', 'GILWD', 'FTVERM', 'MUSKEG', 'GRANW', 'SLPT', 'SWANH', 'WATTMT')")
  )$X_UWI_DISPLAY)
  well_list <- unique(well_list)
  products <- c("Oil") 
} 
if(ResourcePlay== "Spirit River" && Welltype == "Horizontal" ) {
  well_list <- as.factor(rbind(
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN, LEGAL_DLS_LOC where PDEN.PDEN_ID=WELL.UWI and LEGAL_DLS_LOC.UWI=WELL.UWI and WELL.PROVINCE_STATE in ('AB') and WELL.PROFILE_TYPE='H' and WELL.X_ONPROD_DATE > '2007-12-31' and LEGAL_DLS_LOC.DLS_MERIDIAN in (5,6) and PDEN.STRAT_UNIT_ID in ('BULLHD', 'FALHER', 'FALHERA', 'FALHERB', 'FALHERC', 'FALHERD', 'FALHERE', 'NOTIK', 'SPIRIT', 'MANN', 'UMANN', 'WILRICH') and PDEN.PRIMARY_PRODUCT not in ('CBM', 'CBM-OTHER', 'CRUDE-BIT', 'STEAM', 'WATER')")
  )$X_UWI_DISPLAY)
  well_list <- unique(well_list)
  products <- c("Gas") 
} 
if(ResourcePlay== "Viking" && Welltype == "Horizontal" ) {
  well_list <- as.factor(rbind(
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN where PDEN.PDEN_ID=WELL.UWI and WELL.PROVINCE_STATE='SK' and WELL.PROFILE_TYPE='H' and PDEN.PRIMARY_PRODUCT='CRUDE-OIL' and PDEN.STRAT_UNIT_ID in ('VIK', 'VIKSS')"),
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN, LEGAL_DLS_LOC where PDEN.PDEN_ID=WELL.UWI and LEGAL_DLS_LOC.UWI=WELL.UWI and WELL.PROVINCE_STATE='AB' and WELL.PROFILE_TYPE='H' and PDEN.PRIMARY_PRODUCT='CRUDE-OIL' and PDEN.STRAT_UNIT_ID in ('VIK', 'VIKSS') and LEGAL_DLS_LOC.DLS_TOWNSHIP in ('055', '056', '057', '058') and LEGAL_DLS_LOC.DLS_RANGE in ('19', '20', '21', '22', '23')")
  )$X_UWI_DISPLAY)
  well_list <- unique(well_list)
  products <- c("Oil") 
} 
if(ResourcePlay== "Horseshoe Canyon" && Welltype == "Vertical" ) {
  well_list <- as.factor(rbind(
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN where PDEN.PDEN_ID=WELL.UWI and WELL.PROVINCE_STATE in ('AB', 'SK') and PDEN.STRAT_UNIT_ID in ('MEDHAT', 'MILKR', 'LMILKR', 'UMILKR')")
  )$X_UWI_DISPLAY)
  well_list <- unique(well_list)
  #length(well_list)
  #sqlFetch(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "LEGAL_NTS_LOC", max=25)
  #sqlTables(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), tableName = "L%")
  #temp <- sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select PDEN.STRAT_UNIT_ID from WELL, PDEN where PDEN.PDEN_ID=WELL.UWI and WELL.PROVINCE_STATE in ('BC') and WELL.PROFILE_TYPE='H'")
  #temp <- sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.TD_STRAT_UNIT_ID from WELL, PDEN where PDEN.PDEN_ID=WELL.UWI and WELL.PROVINCE_STATE in ('AB') and WELL.X_ONPROD_DATE > '2007-12-31' and WELL.PROFILE_TYPE='H'")
  well_list <- read.csv("Well_List_HSC_All.csv")[,1]
  products <- c("Gas") 
} 
if(ResourcePlay== "Shallow Gas" && Welltype == "Vertical" ) {
  well_list <- as.factor(rbind(
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN where PDEN.PDEN_ID=WELL.UWI and WELL.PROVINCE_STATE in ('AB', 'SK') and PDEN.STRAT_UNIT_ID in ('MEDHAT', 'MILKR', 'LMILKR', 'UMILKR')")
  )$X_UWI_DISPLAY)
  well_list <- unique(well_list)
  #length(well_list)
  #sqlFetch(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "LEGAL_NTS_LOC", max=25)
  #sqlTables(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), tableName = "L%")
  #temp <- sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select PDEN.STRAT_UNIT_ID from WELL, PDEN where PDEN.PDEN_ID=WELL.UWI and WELL.PROVINCE_STATE in ('BC') and WELL.PROFILE_TYPE='H'")
  #temp <- sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.TD_STRAT_UNIT_ID from WELL, PDEN where PDEN.PDEN_ID=WELL.UWI and WELL.PROVINCE_STATE in ('AB') and WELL.X_ONPROD_DATE > '2007-12-31' and WELL.PROFILE_TYPE='H'")
  products <- c("Gas") 
} 
if(ResourcePlay== "Saskatchewan" && Welltype == "Undefined" ) {
  well_list <- as.factor(rbind(
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN where PDEN.PDEN_ID=WELL.UWI and WELL.PROVINCE_STATE in ('SK')")
  )$X_UWI_DISPLAY)
  well_list <- unique(well_list)
  mongodb.well <- mongo(collection = "well", url = paste("mongodb://", mongo_data_base$user, ":", mongo_data_base$pwd, "@", mongo_data_base$host, "/",mongo_data_base$dbname,sep="")  )
  wells_otherplays <- mongodb.well$distinct(
    "UWI",
    toJSON( 
      list(
        ResourcePlay = list(
          "$ne" = ResourcePlay
        )
      ),
      auto_unbox=TRUE
    )
  )
  well_list <- well_list[!(well_list %in% wells_otherplays)]
  rm(mongodb.well, wells_otherplays)
  products <- c("Gas", "Oil") 
} 
if(ResourcePlay== "Cadomin" && Welltype == "Undefined" ) {
  well_list <- as.factor(rbind(
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN where PDEN.PDEN_ID=WELL.UWI and WELL.PROVINCE_STATE in ('AB', 'SK') and PDEN.STRAT_UNIT_ID in ('MEDHAT', 'MILKR', 'LMILKR', 'UMILKR')")
  )$X_UWI_DISPLAY)
  well_list <- unique(well_list)
  #length(well_list)
  #sqlFetch(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "LEGAL_NTS_LOC", max=25)
  #sqlTables(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), tableName = "L%")
  #temp <- sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select PDEN.STRAT_UNIT_ID from WELL, PDEN where PDEN.PDEN_ID=WELL.UWI and WELL.PROVINCE_STATE in ('BC') and WELL.PROFILE_TYPE='H'")
  #temp <- sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.TD_STRAT_UNIT_ID from WELL, PDEN where PDEN.PDEN_ID=WELL.UWI and WELL.PROVINCE_STATE in ('AB') and WELL.X_ONPROD_DATE > '2007-12-31' and WELL.PROFILE_TYPE='H'")
  well_list <- read.csv("Well_List_Cadomin_All.csv")[,1]
  products <- c("Gas") 
} 
if(ResourcePlay== "Montney" && Welltype == "Undefined" ) {
  well_list <- as.factor(rbind(
    sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.X_UWI_DISPLAY from WELL, PDEN where PDEN.PDEN_ID=WELL.UWI and WELL.PROVINCE_STATE in ('AB', 'SK') and PDEN.STRAT_UNIT_ID in ('MEDHAT', 'MILKR', 'LMILKR', 'UMILKR')")
  )$X_UWI_DISPLAY)
  well_list <- unique(well_list)
  #length(well_list)
  #sqlFetch(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "LEGAL_NTS_LOC", max=25)
  #sqlTables(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), tableName = "L%")
  #temp <- sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select PDEN.STRAT_UNIT_ID from WELL, PDEN where PDEN.PDEN_ID=WELL.UWI and WELL.PROVINCE_STATE in ('BC') and WELL.PROFILE_TYPE='H'")
  #temp <- sqlQuery(odbcConnect(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), "select WELL.TD_STRAT_UNIT_ID from WELL, PDEN where PDEN.PDEN_ID=WELL.UWI and WELL.PROVINCE_STATE in ('AB') and WELL.X_ONPROD_DATE > '2007-12-31' and WELL.PROFILE_TYPE='H'")
  well_list <- read.csv("Well_List_All_Montney.csv")[,1]
  products <- c("Gas") 
} 
if(FALSE){
  #well_list <- read.csv("Well_List_All_Shallow_Gas.csv")[,1]
  #well_list <- read.csv("Alberta Montney Hz (2016-09-23).csv")[,1]
  #well_list <- read.csv("BC Montney Hz (2016-09-23).csv")[,1]
  #well_list <- read.csv("Well_List_Swan_Hills_Hz.csv")[,1]
}
if(data_base$vendor =="PPDM") well_list <- capitalize(as.character(levels(well_list))[well_list])
well_list <- well_list[!duplicated(well_list)]

Update_ResourcePlay(mongo_data_base, well_list, ResourcePlay, CPU_count)




#########################################################
# Define colour palettes and number of intervals to use #
#########################################################

num_intervals <- 6
pal_blue <- brewer.pal(3,"Blues") #define bins of colors, blues in this case
pal_red <- brewer.pal(3,"Reds") #define bins of colors, reds in this case
pal_green <- brewer.pal(3,"Greens") #define bins of colors, greens in this case
pal_accent <- brewer.pal(3,"Accent") #define bins of colors, blues in this case

if(FALSE){
  k<-1
  timestep<-1
  j<-1
  #products <- c("Oil")  # choose from Gas, Oil (just these two for now... water and cond can be added as necessary)
  load.ffdf(dir=paste(workdir, "ff_well_data_", ResourcePlay, sep=""))
  load.ffdf(dir=paste(workdir, "ff_prod_data_", ResourcePlay, sep=""))
  load.ffdf(dir=paste(workdir, "ff_frac_data_", ResourcePlay, sep=""))
  wells<-levels(well_data$X_UWI_DISPLAY)
  group <- vector("list", 1)
  group[[k]]<-wells
}

#######################
# Loop for all Groups #
#######################


group <- vector("list", (length(well_list)%/%max_wells_process + if(length(well_list)%%max_wells_process > 0) 1 else 0))
group[[1]] <- well_list[1:min(length(well_list),max_wells_process)]
l=1
while(length(well_list)>max_wells_process*l) {
  group[[l+1]] <- well_list[(1:min((length(well_list)-max_wells_process*l),max_wells_process))+max_wells_process*l]
  l <- l+1
}
rm(l)


#23 groups 6,10,36,42,46
for(k in 1:length(group)){
  #for(k in 5:5){
  #for(k in length(group):1){
  
  
  wells <- group[[k]]
  if(data_base$vendor =="HPDI") wells<-sort(wells)
  
  # Query All Wells at once
  temp <- wells[1:min(length(wells),max_wells_dbquery)]
  
  well_data <- read.odbc.ffdf(query=paste(well_query, paste(temp, collapse = "', '"), "') ",sep=""),
                              odbcConnect.args=list(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd),VERBOSE=TRUE)
  
  #well_data <- read.odbc.ffdf(query="select * from WELL", odbcConnect.args=list(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd),VERBOSE=TRUE)
  #write.csv.ffdf(well_data, file="well_data.csv")
  
  if(data_base$vendor =="PPDM") {
    prod_data <- read.odbc.ffdf(query=paste(prod_query, paste(temp, collapse = "', '"), "') order by PDEN_ID, PROD_DATE",sep=""),
                                odbcConnect.args=list(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd),VERBOSE=TRUE)
    
    #prod_data <- read.odbc.ffdf(query="select * from PDEN_PRODUCTION_MONTH", odbcConnect.args=list(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd),VERBOSE=TRUE)
    #write.csv.ffdf(prod_data, file="prod_data.csv")
  }
  
  if(data_base$vendor =="HPDI") {
    prod_data <- read.odbc.ffdf(query=paste(prod_query, paste(temp, collapse = "', '"), "') order by ENTITY_ID, PROD_DATE",sep=""),
                                odbcConnect.args=list(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd),VERBOSE=TRUE)
  }
  
  
  
  
  if(data_base$vendor =="PPDM") well_query <- "select WELL.X_UWI_DISPLAY, WELL.UWI, WELL.SURFACE_LATITUDE, WELL.SURFACE_LONGITUDE, WELL.X_TD_TVD, WELL.DRILL_TD, WELL.X_ONPROD_DATE, WELL.RIG_RELEASE_DATE, WELL.PROFILE_TYPE, GLJ_PDEN_SUMMARY.PSUM_POOL_NAME, GLJ_PDEN_SUMMARY.PSUM_OPERATOR_NAME from WELL, GLJ_PDEN_SUMMARY where GLJ_PDEN_SUMMARY.PSUM_UWI=WELL.UWI and WELL.X_UWI_DISPLAY in ('"
  if(data_base$vendor =="PPDM") prod_query <- "select WELL.X_UWI_DISPLAY, WELL.X_TD_TVD, PDEN_PRODUCTION_MONTH.PROD_DATE, PDEN_PRODUCTION_MONTH.GAS, PDEN_PRODUCTION_MONTH.WATER, PDEN_PRODUCTION_MONTH.OIL_BT, PDEN_PRODUCTION_MONTH.COND, PDEN_PRODUCTION_MONTH.CUM_GAS, PDEN_PRODUCTION_MONTH.CUM_OIL_BT, PDEN_PRODUCTION_MONTH.CUM_WATER, PDEN_PRODUCTION_MONTH.CUM_COND, PDEN_PRODUCTION_MONTH.TOTAL_FLUID, PDEN_PRODUCTION_MONTH.GAS_CAL_DAY, PDEN_PRODUCTION_MONTH.OIL_CAL_DAY, PDEN_PRODUCTION_MONTH.WATER_CAL_DAY, PDEN_PRODUCTION_MONTH.COND_CAL_DAY, PDEN_PRODUCTION_MONTH.TOTAL_FLUID_CAL_DAY, PDEN_PRODUCTION_MONTH.GAS_ACT_DAY, PDEN_PRODUCTION_MONTH.OIL_ACT_DAY, PDEN_PRODUCTION_MONTH.WATER_ACT_DAY, PDEN_PRODUCTION_MONTH.COND_ACT_DAY, PDEN_PRODUCTION_MONTH.TOTAL_FLUID_ACT_DAY from WELL, PDEN_PRODUCTION_MONTH where PDEN_PRODUCTION_MONTH.PDEN_ID=WELL.UWI and WELL.X_UWI_DISPLAY in ('"
  
  
  
  #well_data <- read.odbc.ffdf(query="select * from WELL", odbcConnect.args=list(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd),VERBOSE=TRUE)
  #prod_data <- read.odbc.ffdf(query="select * from PDEN_PRODUCTION_MONTH",odbcConnect.args=list(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd),VERBOSE=TRUE)
  #frac_data <- read.odbc.ffdf(query="select * from FRAC_MASTER",odbcConnect.args=list(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd),VERBOSE=TRUE)
  #PDEN <- read.odbc.ffdf(query="select * from PDEN", odbcConnect.args=list(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), first.rows = 500000, next.rows = 500000, VERBOSE=TRUE)
  #LEGAL_DLS_LOC <- read.odbc.ffdf(query="select * from LEGAL_DLS_LOC", odbcConnect.args=list(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), VERBOSE=TRUE)

  #save.ffdf(well_data, dir=paste(workdir, "ff_well_data_", ResourcePlay, sep=""), overwrite=TRUE)
  #save.ffdf(prod_data, dir=paste(workdir, "ff_prod_data_", ResourcePlay, sep=""), overwrite=TRUE)
  #save.ffdf(frac_data, dir=paste(workdir, "ff_frac_data_", ResourcePlay, sep=""), overwrite=TRUE)
  #load.ffdf(dir=paste(workdir, "ff_well_data_", ResourcePlay, sep=""))
  #load.ffdf(dir=paste(workdir, "ff_prod_data_", ResourcePlay, sep=""))
  #load.ffdf(dir=paste(workdir, "ff_frac_data_", ResourcePlay, sep=""))
  #wells<-levels(well_data$X_UWI_DISPLAY)
  #group <- vector("list", 1)
  #group[[k]]<-wells
  
  #form_data <- read.odbc.ffdf(query=paste(form_query, paste(temp, collapse = "', '"), "') ",sep=""),
  #                            odbcConnect.args=list(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd),VERBOSE=TRUE)
  
  l=1
  while(length(wells)>max_wells_dbquery*l) {
    temp <- wells[(1:min((length(wells)-max_wells_dbquery*l),max_wells_dbquery))+max_wells_dbquery*l]
    temp3 <- read.odbc.ffdf(query=paste(well_query, paste(temp, collapse = "', '"), "')",sep=""), odbcConnect.args=list(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), VERBOSE=TRUE)
    tryCatch(
      well_data<-ffdfappend(well_data,temp3,adjustvmode=TRUE),
      error=function(l) {
        for(i in 1:length(well_data)) {
          if(!(vmode(temp3[i])==vmode(well_data[i]))) {
            temp3[i] <- as.ff(temp3[i], vmode=vmode(well_data[i]))
          }
        }
        well_data<-ffdfappend(well_data,temp3,adjustvmode=TRUE)
      }
    )
    if(data_base$vendor =="PPDM") {
      temp2 <- read.odbc.ffdf(query=paste(prod_query, paste(temp, collapse = "', '"), "') order by PDEN_ID, PROD_DATE",sep=""), odbcConnect.args=list(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), VERBOSE=TRUE)
    }
    if(data_base$vendor =="HPDI") {
      temp2 <- read.odbc.ffdf(query=paste(prod_query, paste(temp, collapse = "', '"), "') order by ENTITY_ID, PROD_DATE",sep=""), odbcConnect.args=list(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), VERBOSE=TRUE)
    }
    tryCatch(
      if(dim(temp2)[1]>0) {
        prod_data<-ffdfappend(prod_data,temp2,adjustvmode=TRUE)
        },
      error=function(l) {
        for(i in 1:length(prod_data)) {
          if(!(vmode(temp2[i])==vmode(prod_data[i]))) {
            temp2[i] <- as.ff(temp2[i], vmode=vmode(prod_data[i]))
          }
          }
        if(dim(temp2)[1]>0) {
          prod_data<-ffdfappend(prod_data,temp2,adjustvmode=TRUE)
        }
      }
    )
    
    
    #form_data<-ffdfappend(form_data,read.odbc.ffdf(query=paste(form_query, paste(temp, collapse = "', '"), "')",sep=""), odbcConnect.args=list(dsn=data_base$dsn,uid=data_base$uid, pwd=data_base$pwd), VERBOSE=TRUE)[,],adjustvmode=TRUE)
    l <- l+1
  }
  
  
  if(data_base$vendor =="HPDI") {
    well_data <- as.ffdf(renameColumns(well_data[,], from = c("ENTITY_ID", "API_NO", "LATITUDE", "LONGITUDE", "LOWER_PERF", "TOTAL_DEPTH", "FIRST_PROD_DATE", "COMP_DATE", "RESERVOIR", "CURR_OPER_NAME"), to = c("X_UWI_DISPLAY", "UWI", "SURFACE_LATITUDE", "SURFACE_LONGITUDE", "X_TD_TVD", "DRILL_TD", "X_ONPROD_DATE", "RIG_RELEASE_DATE", "PSUM_POOL_NAME", "PSUM_OPERATOR_NAME")))
    prod_data <- as.ffdf(renameColumns(prod_data[,], from = c("ENTITY_ID", "TOTAL_DEPTH", "WTR", "LIQ"), to = c("X_UWI_DISPLAY", "X_TD_TVD", "WATER", "OIL_BT")))
    temp <- as.ffdf(data.frame(COND=rep(NA_real_, length(prod_data$X_UWI_DISPLAY)), CUM_COND=rep(NA_real_, length(prod_data$X_UWI_DISPLAY)), TOTAL_FLUID=rep(NA_real_, length(prod_data$X_UWI_DISPLAY)), COND_CAL_DAY=rep(NA_real_, length(prod_data$X_UWI_DISPLAY)), TOTAL_FLUID_CAL_DAY=rep(NA_real_, length(prod_data$X_UWI_DISPLAY)), GAS_ACT_DAY=rep(NA_real_, length(prod_data$X_UWI_DISPLAY)), OIL_ACT_DAY=rep(NA_real_, length(prod_data$X_UWI_DISPLAY)), WATER_ACT_DAY=rep(NA_real_, length(prod_data$X_UWI_DISPLAY)), COND_ACT_DAY=rep(NA_real_, length(prod_data$X_UWI_DISPLAY)), TOTAL_FLUID_ACT_DAY=rep(NA_real_, length(prod_data$X_UWI_DISPLAY))))
    prod_data <- as.ffdf(cbind(prod_data[,], temp[,]))
    
    well_data$X_TD_TVD <- well_data$X_TD_TVD*0.3048
    well_data$DRILL_TD <- well_data$DRILL_TD*0.3048
    prod_data$X_TD_TVD <- prod_data$X_TD_TVD*0.3048
    prod_data$GAS <- prod_data$GAS/35.494
    prod_data$WATER <- prod_data$WATER/6.2898
    prod_data$OIL_BT <- prod_data$OIL_BT/6.2898
    prod_data$COND_CAL_DAY[][is.na(prod_data$COND_CAL_DAY)[]] <- 0
    
    
    #temp <- as.ffdf(ddply(prod_data[,], .(X_UWI_DISPLAY), HPDI_Cum_Sum, .parallel = TRUE))
    cl <- makeCluster(rep('localhost',CPU_count), type = "SOCK")
    registerDoSNOW(cl)
    clusterExport(cl,c("days_in_month")) 
    temp <- as.ffdf(ddply(prod_data[,], .(X_UWI_DISPLAY), HPDI_Cum_Sum, .parallel = TRUE))
    stopCluster(cl)
    
    #pay particular attention to the order of the wells in the next command... had to add a sort command a few lines up
    prod_data <- as.ffdf(cbind(prod_data[,], temp[,2:length(temp)]))
    
    #temp <- as.ffdf(ddply(prod_data[,], .(X_UWI_DISPLAY), HPDI_Cal_Rates, .parallel = TRUE))
    cl <- makeCluster(rep('localhost',CPU_count), type = "SOCK")
    registerDoSNOW(cl)
    clusterExport(cl,c("days_in_month")) 
    temp <- as.ffdf(ddply(prod_data[,], .(X_UWI_DISPLAY), HPDI_Cal_Rates, .parallel = TRUE))
    stopCluster(cl)
    
    #pay particular attention to the order of the wells in the next command... had to add a sort command a few lines up
    prod_data <- as.ffdf(cbind(prod_data[,], temp[,2:length(temp)]))
  }
  
  if(data_base$vendor =="PPDM") {
    #Exclude all data before 1962 as all we have are cumulate to date for prior time periods
    prod_data <- subset(prod_data,PROD_DATE>strptime("1962-01-01","%Y-%m-%d"))
    levels(well_data$PROFILE_TYPE) <- c(levels(well_data$PROFILE_TYPE), "Horizontal", "Vertical") 
    if(length(well_data$PROFILE_TYPE[well_data$PROFILE_TYPE=="H"][,])>0) well_data$PROFILE_TYPE[well_data$PROFILE_TYPE=="H"][,] <- "Horizontal"
    if(length(well_data$PROFILE_TYPE[well_data$PROFILE_TYPE=="V"][,])>0) well_data$PROFILE_TYPE[well_data$PROFILE_TYPE=="V"][,] <- "Vertical"
    if(length(well_data$PROFILE_TYPE[well_data$PROFILE_TYPE=="D"][,])>0) well_data$PROFILE_TYPE[well_data$PROFILE_TYPE=="D"][,] <- "Vertical"
    well_data$PROFILE_TYPE <- droplevels(well_data$PROFILE_TYPE)
    #suppresWarnigns(expr)
    #assign("last.warning",NULL,envir=baseenv())
  }
  
  rm(l,temp,temp2,temp3)
  gc()
  
  
  #Update the well list with what was found in the database
  if(data_base$vendor =="PPDM") missing_wells <- setdiff(wells, levels(well_data$X_UWI_DISPLAY))
  if(data_base$vendor =="HPDI") missing_wells <- setdiff(wells, well_data$X_UWI_DISPLAY[,])
  
  
  if (length(missing_wells)>0) {
    write.csv(missing_wells, file = paste("missing_wells_group_", formatC(k, digits=2, big.mark = ",", format = "d"), ".csv", sep="")) 
    if(data_base$vendor =="PPDM")  wells <- intersect(wells, levels(well_data$X_UWI_DISPLAY))
    if(data_base$vendor =="HPDI")  wells <- intersect(wells, well_data$X_UWI_DISPLAY[,])
  } else {
    print(paste("No missing wells in group", formatC(k, digits=2, big.mark = ",", format = "d")))
  }
  
  
  
  save.ffdf(well_data, dir=paste(workdir, "ff_well_data_", ResourcePlay, sep=""), overwrite=TRUE)
  save.ffdf(prod_data, dir=paste(workdir, "ff_prod_data_", ResourcePlay, sep=""), overwrite=TRUE)
  save.ffdf(frac_data, dir=paste(workdir, "ff_frac_data_", ResourcePlay, sep=""), overwrite=TRUE)
  #load.ffdf(dir=paste(workdir, "ff_well_data_", ResourcePlay, sep=""))
  #load.ffdf(dir=paste(workdir, "ff_prod_data_", ResourcePlay, sep=""))
  #load.ffdf(dir=paste(workdir, "ff_frac_data_", ResourcePlay, sep=""))
  
  #############################################################################
  # Can loop this data exclusion to build our historical dataset
  # prod_data <- subset(prod_data,PROD_DATE<strptime("2014-10-01","%Y-%m-%d"))
  #############################################################################
  
  if(calculate_all_dates) {
    dates <- sort(unique(prod_data[,3]), decreasing = TRUE)
  } else {
    if(calculate_many_dates) {
      dates <- sort(unique(prod_data[,3]), decreasing = TRUE)[1:12]
    } else {
      dates <- sort(unique(prod_data[,3]), decreasing = TRUE)[1]
    }
  }
  
  for(timestep in 1:length(dates)) { 
    #for(timestep in 3:length(dates)) {
    #for(timestep in length(dates):2) {
    
    for(j in 1:length(products)) {
      # Build the results table and fill in the well names and surface locations... use NAs for everything else
      temp <- as.ffdf(
        data.frame(
          UWI=well_data$X_UWI_DISPLAY[,], 
          peak_rate=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          peak_quarter=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          EUR_exp=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          EUR_harm=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          Rem_exp=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          Rem_harm=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          D_exp=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          D_harm=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          qi_exp=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          qi_harm=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          qf=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          D_1=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          D_2=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          D_3=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          D_5=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          D_10=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          D_20=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          D_30=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          AlgorithmReleaseDate=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          Product=rep(factor("Empty", levels=unique(c("Empty",products))), length(well_data$X_UWI_DISPLAY))
        )
      )
      
      temp2 <- as.ffdf(
        data.frame(
          UWI=well_data$X_UWI_DISPLAY[,], 
          disc_break_1=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          disc_slope_1=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          disc_slope_2=rep(NA_real_, length(well_data$X_UWI_DISPLAY)),
          disc_int_1=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          disc_int_2=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          disc_error_1=rep(NA_real_, length(well_data$X_UWI_DISPLAY)),  
          disc_error_2=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          cont_break_1=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          cont_error_1=rep(NA_real_, length(well_data$X_UWI_DISPLAY)), 
          EUR=rep(NA_real_, length(well_data$X_UWI_DISPLAY)),
          Product=rep(factor("Empty", levels=unique(c("Empty",products))), length(well_data$X_UWI_DISPLAY))  
        )
      )  
      
      temp3 <- as.ffdf(
        data.frame(
          UWI=well_data$X_UWI_DISPLAY[,], 
          peak_CGR=rep(NA_real_, length(well_data$X_UWI_DISPLAY)),
          WGR1=rep(NA_real_, length(well_data$X_UWI_DISPLAY)),
          WGR2=rep(NA_real_, length(well_data$X_UWI_DISPLAY)),
          WGR3=rep(NA_real_, length(well_data$X_UWI_DISPLAY)),
          Product=rep(factor("Empty", levels=unique(c("Empty",products))), length(well_data$X_UWI_DISPLAY))
        )
      )  
      
      if(products[j]=="Gas") {
        #Fill in the data for peak rate and best quarter rate (gas only)
        #peak_rates <- ffdfdply(subset(prod_data,PROD_DATE<=dates[timestep])[c("X_UWI_DISPLAY","GAS_CAL_DAY")], split = subset(prod_data,PROD_DATE<=dates[timestep])$X_UWI_DISPLAY, FUN=function(x){summaryBy(GAS_CAL_DAY ~ X_UWI_DISPLAY, data=x, FUN=max, keep.names=FALSE)}, trace=FALSE)[,]     
        peak_rates <- as.data.frame(do.call(rbind,lapply(split(subset(prod_data,PROD_DATE<=dates[timestep])[,],subset(prod_data,PROD_DATE<=dates[timestep])$X_UWI_DISPLAY[,]),function(x) max(x$GAS_CAL_DAY))))
        names(peak_rates) <- "GAS_CAL_DAY.max"
        peak_rates$X_UWI_DISPLAY <- row.names(peak_rates)
        #peak_quarters <- ffdfdply(subset(prod_data,PROD_DATE<=dates[timestep])[c("X_UWI_DISPLAY","GAS_CAL_DAY")], split = subset(prod_data,PROD_DATE<=dates[timestep])$X_UWI_DISPLAY, FUN=function(x){summaryBy(GAS_CAL_DAY ~ X_UWI_DISPLAY, data=x, FUN=Peak_Quarter, keep.names=FALSE)}, trace=FALSE)[,]  
        try(peak_quarters <- as.data.frame(do.call(rbind,lapply(split(subset(prod_data,PROD_DATE<=dates[timestep])[,],subset(prod_data,PROD_DATE<=dates[timestep])$X_UWI_DISPLAY[,]),function(x){summaryBy(GAS_CAL_DAY ~ X_UWI_DISPLAY, data=x, FUN=Peak_Quarter, keep.names=FALSE)}))),TRUE)
        if(!exists("peak_quarters")) {peak_quarters <- ffdfdply(subset(prod_data,PROD_DATE<=dates[timestep])[c("X_UWI_DISPLAY","GAS_CAL_DAY")], split = subset(prod_data,PROD_DATE<=dates[timestep])$X_UWI_DISPLAY, FUN=function(x){summaryBy(GAS_CAL_DAY ~ X_UWI_DISPLAY, data=x, FUN=Peak_Quarter, keep.names=FALSE)}, trace=FALSE)[,]}
        temp$peak_rate[match(peak_rates$X_UWI_DISPLAY, temp$UWI[,])] <- 35.494*peak_rates$GAS_CAL_DAY.max
        temp$peak_quarter[match(peak_quarters$X_UWI_DISPLAY, temp$UWI[,])] <- 35.494*peak_quarters$GAS_CAL_DAY.Peak_Quarter
      }
      
      if(products[j]=="Oil") {  
        #Fill in the data for peak rate and best quarter rate (oil only)
        #peak_rates <- ffdfdply(subset(prod_data,PROD_DATE<=dates[timestep])[c("X_UWI_DISPLAY","OIL_CAL_DAY")], split = subset(prod_data,PROD_DATE<=dates[timestep])$X_UWI_DISPLAY, FUN=function(x){summaryBy(OIL_CAL_DAY ~ X_UWI_DISPLAY, data=x, FUN=max, keep.names=FALSE)}, trace=FALSE)[,]
        peak_rates <- as.data.frame(do.call(rbind,lapply(split(subset(prod_data,PROD_DATE<=dates[timestep])[,],subset(prod_data,PROD_DATE<=dates[timestep])$X_UWI_DISPLAY[,]),function(x) max(x$OIL_CAL_DAY))))
        names(peak_rates) <- "OIL_CAL_DAY.max"
        peak_rates$X_UWI_DISPLAY <- row.names(peak_rates)
        #peak_quarters <- ffdfdply(subset(prod_data,PROD_DATE<=dates[timestep])[c("X_UWI_DISPLAY","OIL_CAL_DAY")], split = subset(prod_data,PROD_DATE<=dates[timestep])$X_UWI_DISPLAY, FUN=function(x){summaryBy(OIL_CAL_DAY ~ X_UWI_DISPLAY, data=x, FUN=Peak_Quarter, keep.names=FALSE)}, trace=FALSE)[,]  
        try(peak_quarters <- as.data.frame(do.call(rbind,lapply(split(subset(prod_data,PROD_DATE<=dates[timestep])[,],subset(prod_data,PROD_DATE<=dates[timestep])$X_UWI_DISPLAY[,]),function(x){summaryBy(OIL_CAL_DAY ~ X_UWI_DISPLAY, data=x, FUN=Peak_Quarter, keep.names=FALSE)}))),TRUE)
        if(!exists("peak_quarters")) {peak_quarters <- ffdfdply(subset(prod_data,PROD_DATE<=dates[timestep])[c("X_UWI_DISPLAY","OIL_CAL_DAY")], split = subset(prod_data,PROD_DATE<=dates[timestep])$X_UWI_DISPLAY, FUN=function(x){summaryBy(OIL_CAL_DAY ~ X_UWI_DISPLAY, data=x, FUN=Peak_Quarter, keep.names=FALSE)}, trace=FALSE)[,]}
        temp$peak_rate[match(peak_rates$X_UWI_DISPLAY, temp$UWI[,])] <- 6.28981*peak_rates$OIL_CAL_DAY.max
        temp$peak_quarter[match(peak_quarters$X_UWI_DISPLAY, temp$UWI[,])] <- 6.28981*peak_quarters$OIL_CAL_DAY.Peak_Quarter
      }  
      
      if(save_yields) {
        peak_CGR <- as.data.frame(
          do.call(
            rbind,
            lapply(
              split(
                subset(subset(prod_data,PROD_DATE<=dates[timestep]),GAS_CAL_DAY>0)[,],
                subset(subset(prod_data,PROD_DATE<=dates[timestep]),GAS_CAL_DAY>0)$X_UWI_DISPLAY[,]
              ),
              function(x) {
                Result <- NA_real_
                try(
                  Result <- max(
                    rollapply(
                      getDataPart(1000*6.28981*(x$OIL_CAL_DAY+x$COND_CAL_DAY)/(35.494*x$GAS_CAL_DAY)), 
                      3, 
                      mean, 
                      partial = TRUE,
                      na.rm = TRUE
                    )
                  ),
                  TRUE
                )
                return(Result)
              }
            )
          )
        )
        names(peak_CGR) <- "CGR.max"
        peak_CGR$X_UWI_DISPLAY <- row.names(peak_CGR)
        temp3$peak_CGR[match(peak_CGR$X_UWI_DISPLAY, temp3$UWI[,])] <- peak_CGR$CGR.max
        
        #Fill the data for the long term (WGR1, WGR2 which excludes first six months) and last 6 month WGR (WGR3)
        WGR <- as.data.frame(
          do.call(
            rbind,
            lapply(
              split(
                subset(subset(prod_data,PROD_DATE<=dates[timestep]),GAS_CAL_DAY>0)[,],
                subset(subset(prod_data,PROD_DATE<=dates[timestep]),GAS_CAL_DAY>0)$X_UWI_DISPLAY[,]
              ),
              function(x) {
                Result <- c(NA_real_, NA_real_, NA_real_)
                xx <- getDataPart(35.494*x$CUM_GAS)/1000
                yy <- getDataPart(1000*6.28981*x$WATER_CAL_DAY/(35.494*x$GAS_CAL_DAY))
                try(
                  WGR1 <- lmsreg(yy~xx, data.frame(xx,yy)),
                  TRUE
                )
                if(exists("WGR1")) {
                  try(
                    Result[1] <- predict(WGR1, data.frame(xx=max(35.494*x$CUM_GAS)/2000), type="response", terms="yy"),
                    TRUE
                  )
                  if(Result[1]<0) {
                    Result[1] <- mean(yy)
                  }
                }
                if(length(xx)>9) {
                  xx2 <- xx[-c(1:6)]
                  yy2 <- yy[-c(1:6)]
                  try(
                    WGR2 <- lmsreg(yy2~xx2, data.frame(xx2,yy2)),
                    TRUE
                  )
                  if(exists("WGR2")) {
                    try(
                      Result[2] <- predict(WGR2, data.frame(xx2=max(35.494*x$CUM_GAS)/2000), type="response", terms="yy2"),
                      TRUE
                    )
                    if(Result[2]<0) {
                      Result[2] <- mean(yy2)
                    }
                  }
                }
                if(length(xx)>=6) {
                  Result[3] <- mean(tail(yy),6)
                }
                return(Result)
              }
            )
          )
        )
        names(WGR) <- c("WGR1","WGR2","WGR3")
        WGR$X_UWI_DISPLAY <- row.names(WGR)
        temp3$WGR1[match(WGR$X_UWI_DISPLAY, temp3$UWI[,])] <- WGR$WGR1
        temp3$WGR2[match(WGR$X_UWI_DISPLAY, temp3$UWI[,])] <- WGR$WGR2
        temp3$WGR3[match(WGR$X_UWI_DISPLAY, temp3$UWI[,])] <- WGR$WGR3
        temp3$Product[match(peak_CGR$X_UWI_DISPLAY, temp3$UWI[,])] <- products[j] 
        
        write.csv(
          subset(temp3, Product==products[j]), 
          file = paste(ResourcePlay, " ", products[j], " Timestep ", timestep, " Liquid Ratios Group ", k, ".csv",sep="")
        ) 
        rm(peak_CGR,WGR)
      }

      temp$Product[match(peak_rates$X_UWI_DISPLAY, temp$UWI[,])] <- products[j]   
      temp2$Product[match(peak_rates$X_UWI_DISPLAY, temp2$UWI[,])] <- products[j] 
      
      if(!exists("well_results")) {  
        well_results <- temp
        seg_results <- temp2
      } else {
        if(j==1) {
          well_results <- temp
          seg_results <- temp2
        } else {
          well_results <- ffdfappend(well_results,temp[,],adjustvmode=TRUE)
          seg_results <- ffdfappend(seg_results,temp2[,],adjustvmode=TRUE)
        }
      }
      rm(peak_rates,peak_quarters)
    }
    
    
    #Clean up the unneeded variables
    rm(temp,temp2,temp3)
    rm(missing_wells)
    gc()
    
    
    
    #############################
    # Loop for Individual Wells #
    #############################
    
    
    x <- vector("list", length(wells)) # create list
    
    for(j in 1:length(products)) {
      # Build a list of the production data required for our decline.  If we are in linux use a multicore algorithm
      cl <- makeCluster(rep('localhost',CPU_count), type = "SOCK")
      registerDoSNOW(cl)
      clusterExport(cl,c("Abandon_Rate_Gas","Abandon_Rate_Oil")) 
      if(products[j]=="Gas") { 
        x <- dlply(subset(prod_data,PROD_DATE<=dates[timestep])[,], .(X_UWI_DISPLAY), Gas_Data, .parallel = TRUE)
      }
      if(products[j]=="Oil") {  
        x <- dlply(subset(prod_data,PROD_DATE<=dates[timestep])[,], .(X_UWI_DISPLAY), Oil_Data, .parallel = TRUE)
      }
      stopCluster(cl)
      rm(cl)
      
      sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
      sfExport("SimplifiedWellDeclineList","Gas_Reserves","Arps_Reserves","mongo_data_base","products","j","x")
      sfLibrary(jsonlite)
      sfLibrary(mongolite)
      temp <- sfSapply(2:length(x), function(i) SimplifiedWellDeclineList(i))
      sfStop()
      
      x <- x[c(1,(2:length(x))[unlist(temp)])]
      
      if(products[j]=="Gas") { 
        AlgorithmReleaseDate <- Gas_Reserves(ReleaseDate=TRUE)
        sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
        sfExport("Gas_Reserves","x")
        sfLibrary(MASS)
        sfLibrary(LambertW)
        temp <- sfClusterApplyLB(1:length(x), function(i) Gas_Reserves(x[i]))
        sfStop()
        
        sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
        sfExport("Arps_Reserves","x")
        sfLibrary(aRpsDCA)
        temp_Arps <- sfClusterApplyLB(1:length(x), function(i) Arps_Reserves(x[i]))
        sfStop()
      }
      if(products[j]=="Oil") { 
        AlgorithmReleaseDate <- Oil_Reserves(ReleaseDate=TRUE)
        sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
        sfExport("Oil_Reserves","x")
        sfLibrary(MASS)
        sfLibrary(LambertW)
        temp <- sfClusterApplyLB(1:length(x), function(i) Oil_Reserves(x[i]))
        sfStop()
        
        sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
        sfExport("Arps_Reserves","x")
        sfLibrary(aRpsDCA)
        temp_Arps <- sfClusterApplyLB(1:length(x), function(i) Arps_Reserves(x[i]))
        sfStop()
      }
      
      if(data_base$vendor =="PPDM") {
        well_results$EUR_exp[match(well_results$UWI[,][][(well_results$Product==products[j])[]],levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,1]
        well_results$EUR_harm[match(well_results$UWI[,][][(well_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,2]
        well_results$Rem_exp[match(well_results$UWI[,][][(well_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,3]
        well_results$Rem_harm[match(well_results$UWI[,][][(well_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,4]
        well_results$D_exp[match(well_results$UWI[,][][(well_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,5]
        well_results$D_harm[match(well_results$UWI[,][][(well_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,6]
        well_results$qi_exp[match(well_results$UWI[,][][(well_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,7]
        well_results$qi_harm[match(well_results$UWI[,][][(well_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,8]
        well_results$qf[match(well_results$UWI[,][][(well_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,9]
        well_results$D_1[match(well_results$UWI[,][][(well_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,10]
        well_results$D_2[match(well_results$UWI[,][][(well_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,11]
        well_results$D_3[match(well_results$UWI[,][][(well_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,12]
        well_results$D_5[match(well_results$UWI[,][][(well_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,13]
        well_results$D_10[match(well_results$UWI[,][][(well_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,14]
        well_results$D_20[match(well_results$UWI[,][][(well_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,15]
        well_results$D_30[match(well_results$UWI[,][][(well_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,16]
        well_results$AlgorithmReleaseDate[match(well_results$UWI[,][][(well_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,17]
      }
      
      if(data_base$vendor =="HPDI") {
        well_results$EUR_exp[match(names(x), well_results$UWI[,][][(well_results$Product==products[j])[]])] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,1]
        well_results$EUR_harm[match(names(x), well_results$UWI[,][][(well_results$Product==products[j])[]])] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,2]
        well_results$Rem_exp[match(names(x), well_results$UWI[,][][(well_results$Product==products[j])[]])] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,3]
        well_results$Rem_harm[match(names(x), well_results$UWI[,][][(well_results$Product==products[j])[]])] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,4]
        well_results$D_exp[match(names(x), well_results$UWI[,][][(well_results$Product==products[j])[]])] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,5]
        well_results$D_harm[match(names(x), well_results$UWI[,][][(well_results$Product==products[j])[]])] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,6]
        well_results$qi_exp[match(names(x), well_results$UWI[,][][(well_results$Product==products[j])[]])] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,7]
        well_results$qi_harm[match(names(x), well_results$UWI[,][][(well_results$Product==products[j])[]])] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,8]
        well_results$qf[match(names(x), well_results$UWI[,][][(well_results$Product==products[j])[]])] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,9]
        well_results$D_1[match(names(x), well_results$UWI[,][][(well_results$Product==products[j])[]])] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,10]
        well_results$D_2[match(names(x), well_results$UWI[,][][(well_results$Product==products[j])[]])] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,11]
        well_results$D_3[match(names(x), well_results$UWI[,][][(well_results$Product==products[j])[]])] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,12]
        well_results$D_5[match(names(x), well_results$UWI[,][][(well_results$Product==products[j])[]])] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,13]
        well_results$D_10[match(names(x), well_results$UWI[,][][(well_results$Product==products[j])[]])] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,14]
        well_results$D_20[match(names(x), well_results$UWI[,][][(well_results$Product==products[j])[]])] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,15]
        well_results$D_30[match(names(x), well_results$UWI[,][][(well_results$Product==products[j])[]])] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,16]
        well_results$AlgorithmReleaseDate[match(names(x), well_results$UWI[,][][(well_results$Product==products[j])[]])] <- matrix(unlist(temp), ncol=17, byrow=TRUE)[,17]
      }
      
      if(k==1 && j==1) {  
        group_results <- merge(well_data, well_results, by.x = "X_UWI_DISPLAY", by.y = "UWI", all.x=FALSE, all.y=FALSE, trace = TRUE)
      } else {
        group_results <- ffdfappend(group_results,merge(well_data, well_results, by.x = "X_UWI_DISPLAY", by.y = "UWI", all.x=FALSE, all.y=FALSE, trace = TRUE)[,],adjustvmode=TRUE)
      }
      
      if(plot_segmented && calculate_segmented) {
        pdf(paste("Segmented Analysis - Exponential Decline - ", products[j], " - Group ", k,".pdf",sep=""), width=11,height=8.5)
        for(i in 1:length(x)) {
          Seg.Well.Anal.Exp(x[i],PLOT=TRUE,iters=1000);
        }
        dev.off()
      }
      
      if(plot_segmented && calculate_segmented) {
        pdf(paste("Segmented Analysis - Harmonic Decline - ", products[j], " - Group ", k,".pdf",sep=""), width=11,height=8.5)
        for(i in 1:length(x)) {
          Seg.Well.Anal.Harm(x[i],PLOT=TRUE,iters=1000);
        }
        dev.off()
      }
      
      if(plot_segmented && calculate_segmented) {
        pdf(paste("Segmented Analysis - Watten Decline - ", products[j], " - Group ", k,".pdf",sep=""), width=11,height=8.5)
        for(i in 1:length(x)) {
          Seg.Well.Anal.Watten(x[i],PLOT=TRUE,iters=1000);
        }
        dev.off()
      }
      
      if(calculate_segmented) {
        sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
        sfExport("Seg.Well.Anal.Exp", "break.lm.full", "segmented.lm.break", "Boot.Exp.Reserves", "boot.lm", "iters", "x")
        sfLibrary(strucchange)
        sfLibrary(segmented)
        temp2 <- sfClusterApplyLB(1:length(x), function(i) Seg.Well.Anal.Exp(x[i],iters=iters))
        sfStop()
        
        sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
        sfExport("Seg.Well.Anal.Harm", "break.lm.full", "segmented.lm.break", "Boot.Harm.Reserves", "boot.lm", "iters", "x")
        sfLibrary(strucchange)
        sfLibrary(segmented)
        temp3 <- sfClusterApplyLB(1:length(x), function(i) Seg.Well.Anal.Harm(x[i],iters=iters))
        sfStop()
        
        sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
        sfExport("Seg.Well.Anal.Watten", "break.lm.full", "segmented.lm.break", "Boot.Watten.Reserves", "boot.lm", "iters", "x")
        sfLibrary(strucchange)
        sfLibrary(segmented)
        temp4 <- sfClusterApplyLB(1:length(x), function(i) Seg.Well.Anal.Watten(x[i],iters=iters))
        sfStop()
      }
      
      if(save_seg_tables && calculate_segmented) {
        temp_a <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE, dimnames=list(names(x), paste("Exp.",c(names(temp2[[1]])[1:10], paste("Bootstrap.",seq(1:iters),sep="")),sep=""))) 
        temp2_a <- matrix(unlist(temp3), ncol=(10+iters), byrow=TRUE, dimnames=list(names(x), paste("Harm.",c(names(temp3[[1]])[1:10], paste("Bootstrap.",seq(1:iters),sep="")),sep=""))) 
        temp3_a <- matrix(unlist(temp4), ncol=(10+iters), byrow=TRUE, dimnames=list(names(x), paste("Watten.",c(names(temp4[[1]])[1:10], paste("Bootstrap.",seq(1:iters),sep="")),sep=""))) 
        temp4_a <- cbind(temp_a, temp2_a, temp3_a)    
        write.csv(temp4_a,file=paste("Segmented Analysis - Group ", k, " ", products[j], ".csv", sep=""))
        rm(temp_a, temp2_a, temp3_a, temp4_a)
      }
      
      if(!(ResourcePlay=="Undefined") && calculate_areas){
        Voronoi <- VoronoiArea(subset(well_data,X_ONPROD_DATE<=dates[timestep]))
      }          
      
      
      # Kalman Filter Code
      if(FALSE) {
        
        ptm <- proc.time()
        for(i in 1:length(x)) {
          data <- list(
            "00/01-01-013-28W1/0" = x[[i]],
            Prior = data.frame(r=0.02, p0=x[[i]]$Q[1], p_ult=x[[i]]$q[1]*1500+tail(x[[i]]$Q,1))
          )
          Kalman_Logistic_Growth(data,PLOT=TRUE)
        }  
        proc.time() - ptm
        
        #Prior = data.frame(r=1/as.numeric(diff(x[[i]]$t)[1]), p0=x[[i]]$Q[1], p_ult=x[[i]]$q[1]*1500+tail(x[[i]]$Q,1))
        
        i=1
        data <- list(
          "00/01-01-013-28W1/0" = x[[i]],
          Prior = data.frame(q0=as.numeric(max(x[[i]]$q)), D0=0.4, Dinf=0.08, n=0.3)
        )
        Kalman_Power_Law(data,PLOT=TRUE)
      }
      
      if(json_save) {
        
        json_list <- vector("list", length(x)) # create list
        
        
        
        if(calculate_segmented) {
          temp_exp <- temp2[i]
          temp_harm <- temp3[i]
          temp_watten <- temp4[i]
        }
        if(calculate_segmented) {
          buf$SegDeclineExp <- list(Method="Exponential", EUR=temp_exp[[1]]$EUR, Rem=(temp_exp[[1]]$EUR-max(x[[1]]$Q)), DiscBreak1=temp_exp[[1]]$disc.breaks1, DiscSlope1=temp_exp[[1]]$disc.slope1, DiscSlope2=temp_exp[[1]]$disc.slope2, DiscInt1=temp_exp[[1]]$disc.int1, DiscInt2=temp_exp[[1]]$disc.int2, DiscErr1=temp_exp[[1]]$disc.err1, DiscErr2=temp_exp[[1]]$disc.err2, ContBreak1=temp_exp[[1]]$cont.breaks, ContBreakBounds1=temp_exp[[1]]$bounds.breaks, EURDistribution=as.list(temp_exp[[1]][11:(iters+10)]))
          buf$SegDeclineHarm <- list(Method="Harmonic", EUR=temp_harm[[1]]$EUR, Rem=(temp_harm[[1]]$EUR-max(x[[1]]$Q)), DiscBreak1=temp_harm[[1]]$disc.breaks1, DiscSlope1=temp_harm[[1]]$disc.slope1, DiscSlope2=temp_harm[[1]]$disc.slope2, DiscInt1=temp_harm[[1]]$disc.int1, DiscInt2=temp_harm[[1]]$disc.int2, DiscErr1=temp_harm[[1]]$disc.err1, DiscErr2=temp_harm[[1]]$disc.err2, ContBreak1=temp_harm[[1]]$cont.breaks, ContBreakBounds1=temp_harm[[1]]$bounds.breaks, EURDistribution=as.list(temp_harm[[1]][11:(iters+10)]))
          buf$SegDeclineWatt <- list(Method="Wattenberger", EUR=temp_watten[[1]]$EUR, Rem=(temp_watten[[1]]$EUR-max(x[[1]]$Q)), DiscBreak1=temp_watten[[1]]$disc.breaks1, DiscSlope1=temp_watten[[1]]$disc.slope1, DiscSlope2=temp_watten[[1]]$disc.slope2, DiscInt1=temp_watten[[1]]$disc.int1, DiscInt2=temp_watten[[1]]$disc.int2, DiscErr1=temp_watten[[1]]$disc.err1, DiscErr2=temp_watten[[1]]$disc.err2, ContBreak1=temp_watten[[1]]$cont.breaks, ContBreakBounds1=temp_watten[[1]]$bounds.breaks, EURDistribution=as.list(temp_watten[[1]][11:(iters+10)]))
          rm(temp_exp,temp_harm,temp_watten)
        }
        #well_results$UWI[match(names(x[i]), well_results$UWI[,][][(well_results$Product==products[j])[]])]
        #well_results$EUR_exp[match(levels(prod_data$X_UWI_DISPLAY[,]), well_results$UWI[,][][(well_results$Product==products[j])[]])]
        
        
        sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
        sfExport("WriteWellDeclineMongo", "x", "j", "products", "ResourcePlay", "calculate_areas", "temp", "temp_Arps", "well_data", "well_results", "mongo_data_base", "AlgorithmReleaseDate", "Voronoi")
        sfLibrary(jsonlite)
        sfLibrary(mongolite)
        sfLibrary(ffbase)
        json_list <- sfSapply(1:length(x), function(i) WriteWellDeclineMongo(i))
        #json_list <- lapply(1:1, WriteWellDeclineMongo)
        sfStop()
        
        #mongo shell commands
        #use gljdata_test
        #db.auth(user,pass)
        #db.base_declines.createIndex( { UWI: 1}, {background: true} )
        #db.base_declines.createIndex( { Date: 1}, {background: true} )
        #db.base_declines.getIndexes()
        #db.base_declines.findOne({DataCurrencyString: {$exists: 1}})
        #db.base_declines.remove({DataCurrencyString: {$exists: 0}})
        #mongoexport --collection collection --out collection.json
        #mongoexport --collection db.base_declines --out base_declines.json
        #mongoexport -d gljdata_test --sort '{UWI: 1}' --skip 9000000 --limit 453223 -u mike -p glj --collection base_declines --out base_declines_09.json
        
        #mongo.get.database.collections(mongodb, mongo_data_base$dbname)
        #mongo.count(mongodb, mongo_data_base$collection, mongo.bson.empty())        
        #mongo.count(mongodb, mongo_data_base$collection, criteria)
        #mongo.find.all(mongodb, mongo_data_base$collection, criteria)
        #mongo.bson.to.list(mongo.find.one(mongodb, mongo_data_base$collection, criteria))
        
        write(
          paste0(paste0(lapply(json_list, function(i) {toJSON(i, ,auto_unbox=TRUE,POSIXt="mongo",null="null",na="null")}), collapse = '\n'), '\n'), 
          paste(Sys.Date(), " ", ResourcePlay, " group", k, " timestep ", timestep, " ", products[j], ".json",sep="")
        )
        rm(json_list)
        gc()
      }
      lapply(1:ceiling(length(x)/1000), function(i) Arps_Forecasts(mongo_data_base, CPU_count=CPU_count, ResourcePlay=ResourcePlay))
      
      
      if(calculate_segmented) {
        if(data_base$vendor =="PPDM") {
          seg_results$disc_break_1[match(seg_results$UWI[,][][(seg_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,1]
          seg_results$disc_slope_1[match(seg_results$UWI[,][][(seg_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,2]
          seg_results$disc_slope_2[match(seg_results$UWI[,][][(seg_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,3]
          seg_results$disc_int_1[match(seg_results$UWI[,][][(seg_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,4]
          seg_results$disc_int_2[match(seg_results$UWI[,][][(seg_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,5]
          seg_results$disc_error_1[match(seg_results$UWI[,][][(seg_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,6]
          seg_results$disc_error_2[match(seg_results$UWI[,][][(seg_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,7]
          seg_results$cont_break_1[match(seg_results$UWI[,][][(seg_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,8]
          seg_results$cont_error_1[match(seg_results$UWI[,][][(seg_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,9]
          seg_results$EUR[match(seg_results$UWI[,][][(seg_results$Product==products[j])[]], levels(prod_data$X_UWI_DISPLAY[,]))] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,10]
        }
        
        if(data_base$vendor =="HPDI") {
          seg_results$disc_break_1[match(names(x), seg_results$UWI[,][][(seg_results$Product==products[j])[]])] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,1]
          seg_results$disc_slope_1[match(names(x), seg_results$UWI[,][][(seg_results$Product==products[j])[]])] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,2]
          seg_results$disc_slope_2[match(names(x), seg_results$UWI[,][][(seg_results$Product==products[j])[]])] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,3]
          seg_results$disc_int_1[match(names(x), seg_results$UWI[,][][(seg_results$Product==products[j])[]])] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,4]
          seg_results$disc_int_2[match(names(x), seg_results$UWI[,][][(seg_results$Product==products[j])[]])] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,5]
          seg_results$disc_error_1[match(names(x), seg_results$UWI[,][][(seg_results$Product==products[j])[]])] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,6]
          seg_results$disc_error_2[match(names(x), seg_results$UWI[,][][(seg_results$Product==products[j])[]])] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,7]
          seg_results$cont_break_1[match(names(x), seg_results$UWI[,][][(seg_results$Product==products[j])[]])] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,8]
          seg_results$cont_error_1[match(names(x), seg_results$UWI[,][][(seg_results$Product==products[j])[]])] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,9]
          seg_results$EUR[match(names(x), seg_results$UWI[,][][(seg_results$Product==products[j])[]])] <- matrix(unlist(temp2), ncol=(10+iters), byrow=TRUE)[,10]
        }
      }
      
      
      if(k==1 && j==1) {  
        group_seg_results <- merge(well_data, seg_results, by.x = "X_UWI_DISPLAY", by.y = "UWI", all.x=FALSE, all.y=FALSE, trace = TRUE)
      } else {
        group_seg_results <- ffdfappend(group_seg_results,merge(well_data, seg_results, by.x = "X_UWI_DISPLAY", by.y = "UWI", all.x=FALSE, all.y=FALSE, trace = TRUE)[,],adjustvmode=TRUE)
      }
      
      #save.ffdf(group_seg_results,dir=paste(workdir, "/group_seg_results", sep=""),overwrite=TRUE)
      #load.ffdf(dir=paste(workdir, "/group_seg_results", sep=""))
      #setwd(workdir)
      #rm(well_results,seg_results)
      if(calculate_segmented) rm(temp2,temp3,temp4, temp_Arps)
      gc()
      
    } #endif for product type
    #UpdateLocationsMongoWellCollection(mongo_data_base, origproj="+proj=longlat +datum=NAD27", newproj="+proj=longlat +datum=WGS84", CPU_count=CPU_count)
    #Arps_Forecasts(mongo_data_base, CPU_count=CPU_count)
    #UpdateMongoWellCollection(mongo_data_base)
    timestep
  } # endif for processing each month
  
  if(FALSE) { #k means stuff
    #please note that column indices are hardcoded... this is not ideal
    #kmeans fails if there are NAs in the data, so we will analyze the data in three batches
    #the first batch has one segment, the second has two, the third has three segments
    well_declines_3 <- na.omit(group_seg_results[c(1,9:10,12:14,18:20)][,])[,1]
    k_means_declines_3 <- scale(group_seg_results[c(9:10,12:14,18:20)][match(well_declines_3, group_seg_results[1][,]),])
    well_declines_2 <- na.omit(group_seg_results[c(1,9,12,13,18,19)][-match(well_declines_3, group_seg_results[1][,]),])[,1]
    k_means_declines_2 <- scale(group_seg_results[c(9,12,13,18,19)][match(well_declines_2, group_seg_results[1][,]),])
    well_declines_1 <- na.omit(group_seg_results[c(1,12,18)][-c(match(well_declines_2, group_seg_results[1][,]),match(well_declines_3, group_seg_results[1][,])),])[,1]
    k_means_declines_1 <- scale(group_seg_results[c(12,18)][match(well_declines_1, group_seg_results[1][,]),])
    #k_means_declines <- scale(group_seg_results[c("disc_break_1", "disc_break_2", "disc_break_3", "disc_slope_1 disc_slope_2", " disc_slope_3", " disc_int_1", " disc_int_2", " disc_int_3", " disc_error_1", " disc_error_2", " disc_error_3")])
    
    
    if(FALSE) {
      ssPlot <- function(data, maxCluster = 9, plotName=NULL) {
        # Initialize within sum of squares
        SSw <- (nrow(data) - 1) * sum(apply(data, 2, var))
        SSw <- vector()
        for (i in 2:maxCluster) {
          SSw[i] <- sum(kmeans(data, iter.max=1e9, centers = i)$withinss)
        }
        plot(1:maxCluster, SSw, type = "b", xlab = "Number of Clusters (AKA Typecurves)", ylab = "Within Groups Sum of Squares",main=plotName)
      }
      ssPlot(k_means_declines_3,maxCluster=20,plotName=paste("Three Decline Stages (", as.character(nrow(k_means_declines_3)), " wells)", sep=""))
      ssPlot(k_means_declines_2,maxCluster=20,plotName=paste("Two Decline Stages (", as.character(nrow(k_means_declines_2)), " wells)",sep=""))
      ssPlot(k_means_declines_1,maxCluster=20,plotName=paste("One Decline Stage (", as.character(nrow(k_means_declines_1)), " wells)",sep=""))
    }
    
    fit_declines_1 <- kmeans(k_means_declines_1, iter.max=1000000,8)
    fit_declines_2 <- kmeans(k_means_declines_2, iter.max=1000000,10)
    fit_declines_3 <- kmeans(k_means_declines_3, iter.max=1000000,12)
    
    write.csv(
      rbind(
        data.frame(
          #PARENT_ENTITY=rep("One Segment Declines",length(fit_declines_1$cluster)),
          ENTITY=paste("One Segment Declines Group",sprintf("%02d",fit_declines_1$cluster)),
          UWI=well_declines_1
        ),
        data.frame(
          #PARENT_ENTITY=rep("Two Segment Declines",length(fit_declines_2$cluster)),
          ENTITY=paste("Two Segment Declines Group",sprintf("%02d",fit_declines_2$cluster)),
          UWI=well_declines_2
        ),
        data.frame(
          #PARENT_ENTITY=rep("Three Segment Declines",length(fit_declines_3$cluster)),
          ENTITY=paste("Three Segment Declines Group",sprintf("%02d",fit_declines_3$cluster)),
          UWI=well_declines_3
        )
      )
      ,file="RWL_WELL_IMPORT.csv")
    
    
    write.csv(
      rbind(
        data.frame(
          PARENT_ENTITY=rep("One Segment Declines",length(fit_declines_1$cluster)),
          CHILD_ENTITY=paste("One Segment Declines Group",sprintf("%02d",fit_declines_1$cluster))
          #CHILD_ENTITY=well_declines_1
        ),
        data.frame(
          PARENT_ENTITY=rep("Two Segment Declines",length(fit_declines_2$cluster)),
          CHILD_ENTITY=paste("Two Segment Declines Group",sprintf("%02d",fit_declines_2$cluster))
          #CHILD_ENTITY=well_declines_2
        ),
        data.frame(
          PARENT_ENTITY=rep("Three Segment Declines",length(fit_declines_3$cluster)),
          CHILD_ENTITY=paste("Three Segment Declines Group",sprintf("%02d",fit_declines_3$cluster))
          #CHILD_ENTITY=well_declines_3
        )
      )
      ,file="RWL_HIERARCHY_IMPORT.csv")
    
    
    
    #delete(x,prod_data)
    rm(prod_data)
    rm(x)
    gc()
    
    
    ###########################
    # Build the Summary Plots #
    ###########################
    
    
    
    #tikz('Dawson_Upper.tex', width=8.0, height=6.0) #for thesis
    pdf(paste("Group ", formatC(k, digits=2, big.mark = ",", format = "d")," IP Summaries.pdf",sep=""), width = 11, height = 8.5) #for memo
    
    par(mfrow=c(2,3),oma=c(0,0,3,0))  # IP summaries
    options(warn=-1)
    
    
    # Make a QQ plot of the peak calendar day rates
    if(data_base$vendor =="PPDM") temp <- well_data$RIG_RELEASE_DATE[match(well_results$UWI[(well_results$peak_rate>0 & !is.na(well_results$peak_rate))[,]], well_data$X_UWI_DISPLAY[,])]
    if(data_base$vendor =="HPDI") temp <- well_data$X_ONPROD_DATE[match(well_results$UWI[(well_results$peak_rate>0 & !is.na(well_results$peak_rate))[,]], well_data$X_UWI_DISPLAY[,])]
    interval_rig_release <- classIntervals(as.numeric(temp), n=num_intervals, style="quantile")  #to define intervals, quantiles or other, over the data range and bin the datapoints into the intervals
    if(data_base$vendor =="PPDM") temp <- well_data$RIG_RELEASE_DATE[match(well_results$UWI[(well_results$peak_rate>0 & !is.na(well_results$peak_rate))[,]], well_data$X_UWI_DISPLAY[,])]
    if(data_base$vendor =="HPDI") temp <- well_data$X_ONPROD_DATE[match(well_results$UWI[(well_results$peak_rate>0 & !is.na(well_results$peak_rate))[,]], well_data$X_UWI_DISPLAY[,])]
    interval_rig_release <- classIntervals(as.numeric(temp), n=num_intervals, style="fixed", fixedBreaks=interval_rig_release$brks)  #to define intervals, quantiles or other, over the data range and bin the datapoints into the intervals
    pal_rig_release <- findColours(interval_rig_release, pal_blue, under="under", over="over", between="-", digits=0, cutlabels=FALSE)
    zz <- qqnorm(log10(well_results$peak_rate[(well_results$peak_rate>0 & !is.na(well_results$peak_rate))[,]]), datax=FALSE, plot.it=FALSE)
    plot(zz$y, zz$x, main="Log-Normal Q-Q Plot - Peak Calendar Day Rates (IP30)", pch=16, col=pal_rig_release, xaxt="n", xlim=c(1,4), yaxt="n", ylim=qnorm(c(0.005,0.995), mean = 0, sd = 1), xlab="Peak Calendar Day Rate (Mcf/d)", ylab="Cumulative Probability")
    ww <- lqs(zz$y, zz$x,method="lms")
    abline(ww, col="Red")
    axis(1,at=(1:4),labels=c("10", "100", "1,000", "10,000"))
    axis.at <- 10^(1:4)
    axis(1, at = log10(1:10 * rep(axis.at[-1] / 10, each = 10)),tcl = -0.5, labels = FALSE,tck=-0.01)
    prob <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.98, 0.99)
    axis(2,at=qnorm(prob, mean = 0, sd = 1),labels=1-prob)
    yy <- qnorm(c(0.1,0.5,0.9), mean = 0, sd = 1)
    xx <- (yy-coefficients(ww)[1])/coefficients(ww)[2]
    text(xx,yy,pos=4,labels=paste(c(" P90 = "," P50 = "," P10 = "), formatC(10*round(10^(xx-1)), digits=0, big.mark = ",", format = "d")))
    legend("topleft", title="Rig Release After", as.character(as.POSIXct(interval_rig_release$brks,origin=as.Date("1970-1-1")), format="%b %Y")[1:num_intervals], col=attr(pal_rig_release, "palette"),pch=16,bg="white")
    rm(pal_rig_release,prob,temp)
    rm(ww,xx,yy,zz)
    
    
    # Make a QQ plot of the peak quarterly rates
    if(data_base$vendor =="PPDM") temp <- well_data$RIG_RELEASE_DATE[match(well_results$UWI[(well_results$peak_quarter>0 & !is.na(well_results$peak_quarter))[,]], well_data$X_UWI_DISPLAY[,])]
    if(data_base$vendor =="HPDI") temp <- well_data$X_ONPROD_DATE[match(well_results$UWI[(well_results$peak_quarter>0 & !is.na(well_results$peak_quarter))[,]], well_data$X_UWI_DISPLAY[,])]
    interval_rig_release <- classIntervals(as.numeric(temp), n=num_intervals, style="quantile")  #to define intervals, quantiles or other, over the data range and bin the datapoints into the intervals
    if(data_base$vendor =="PPDM") temp <- well_data$RIG_RELEASE_DATE[match(well_results$UWI[(well_results$peak_quarter>0 & !is.na(well_results$peak_quarter))[,]], well_data$X_UWI_DISPLAY[,])]
    if(data_base$vendor =="HPDI") temp <- well_data$X_ONPROD_DATE[match(well_results$UWI[(well_results$peak_quarter>0 & !is.na(well_results$peak_quarter))[,]], well_data$X_UWI_DISPLAY[,])]
    interval_rig_release <- classIntervals(as.numeric(temp), n=num_intervals, style="fixed", fixedBreaks=interval_rig_release$brks)  #to define intervals, quantiles or other, over the data range and bin the datapoints into the intervals
    pal_rig_release <- findColours(interval_rig_release, pal_blue, under="under", over="over", between="-", digits=0, cutlabels=FALSE)
    zz <- qqnorm(log10(well_results$peak_quarter[(well_results$peak_quarter>0 & !is.na(well_results$peak_quarter))[,]]), datax=FALSE, plot.it=FALSE)
    plot(zz$y, zz$x, main="Log-Normal Q-Q Plot - Peak Quarterly Rates (IP90)", pch=16, col=pal_rig_release, xaxt="n", xlim=c(1,4), yaxt="n", ylim=qnorm(c(0.005,0.995), mean = 0, sd = 1), xlab="Peak Quarterly Rate (Mcf/d)", ylab="Cumulative Probability")
    ww <- lqs(zz$y, zz$x,method="lms")
    abline(ww, col="Red")
    axis(1,at=(1:4),labels=c("10", "100", "1,000", "10,000"))
    axis.at <- 10^(1:4)
    axis(1, at = log10(1:10 * rep(axis.at[-1] / 10, each = 10)),tcl = -0.5, labels = FALSE,tck=-0.01)
    prob <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.98, 0.99)
    axis(2,at=qnorm(prob, mean = 0, sd = 1),labels=1-prob)
    yy <- qnorm(c(0.1,0.5,0.9), mean = 0, sd = 1)
    xx <- (yy-coefficients(ww)[1])/coefficients(ww)[2]
    text(xx,yy,pos=4,labels=paste(c(" P90 = "," P50 = "," P10 = "), formatC(10*round(10^(xx-1)), digits=0, big.mark = ",", format = "d")))
    legend("topleft", title="Rig Release After", as.character(as.POSIXct(interval_rig_release$brks,origin=as.Date("1970-1-1")), format="%b %Y")[1:num_intervals], col=attr(pal_rig_release, "palette"),pch=16,bg="white")
    rm(pal_rig_release,prob,temp)
    rm(ww,xx,yy,zz)
    rm(axis.at)
    
    
    # Make of plot of best quarter versus peak rate
    if(data_base$vendor =="PPDM") temp <- well_data$RIG_RELEASE_DATE[match(well_results$UWI[(well_results$peak_rate>0 & !is.na(well_results$peak_rate) & well_results$peak_quarter>0 & !is.na(well_results$peak_quarter))[,]], well_data$X_UWI_DISPLAY[,])]
    if(data_base$vendor =="HPDI") temp <- well_data$X_ONPROD_DATE[match(well_results$UWI[(well_results$peak_rate>0 & !is.na(well_results$peak_rate) & well_results$peak_quarter>0 & !is.na(well_results$peak_quarter))[,]], well_data$X_UWI_DISPLAY[,])]
    interval_rig_release <- classIntervals(as.numeric(temp), n=num_intervals, style="quantile")  #to define intervals, quantiles or other, over the data range and bin the datapoints into the intervals
    xx <- well_results$peak_rate[(well_results$peak_quarter>0 & !is.na(well_results$peak_quarter) & well_results$peak_rate>0 & !is.na(well_results$peak_rate))[,]]
    yy <- well_results$peak_quarter[(well_results$peak_quarter>0 & !is.na(well_results$peak_quarter) & well_results$peak_rate>0 & !is.na(well_results$peak_rate))[,]]
    interval_rig_release <- classIntervals(as.numeric(temp), n=num_intervals, style="fixed", fixedBreaks=interval_rig_release$brks)  #to define intervals, quantiles or other, over the data range and bin the datapoints into the intervals
    pal_rig_release <- findColours(interval_rig_release, pal_blue, under="under", over="over", between="-", digits=0, cutlabels=FALSE)
    plot(xx, yy, xlab="Peak Calendar Day Rate (Mcf/d)", ylab="Peak Quarter Rate (Mcf/d)", main=CapLeading(paste("Group", formatC(k, digits=2, big.mark = ",", format = "d"), " Wells",sep="")), pch=16, col=pal_rig_release)
    best_fit <- lm(yy ~ xx -1)
    abline(best_fit, col="red")
    hght <- strheight("Here")
    text(x=par("usr")[2]/2,y=par("usr")[4]*0.925,paste("Peak Quarter Rate is ", formatC(best_fit$coefficients*100, digits=0, big.mark = ",", format = "d"), "% of the Peak Rate", sep=""))
    legend("bottomright", title="Rig Release After", as.character(as.POSIXct(interval_rig_release$brks,origin=as.Date("1970-1-1")), format="%b %Y")[1:num_intervals], col=attr(pal_rig_release, "palette"),pch=16,bg="white")
    rm(best_fit,hght,pal_rig_release,temp)
    rm(xx,yy)
    
    title(main=paste("Summary of Initial Rates for Group ", formatC(k, digits=2, big.mark = ",", format = "d"), " Wells",sep=""),outer=TRUE)
    dev.off() # IP summaries
    
    
    
    
    
    #tikz('Dawson_Upper.tex', width=8.0, height=6.0) #for thesis
    pdf(paste("Group ", formatC(k, digits=2, big.mark = ",", format = "d")," EUR Correlations.pdf",sep=""), width = 11, height = 8.5) #for memo
    
    par(mfrow=c(2,3),oma=c(0,0,3,0))  # EUR Correlations
    options(warn=-1)
    
    
    # Make a QQ plot of the EURs
    if(data_base$vendor =="PPDM") temp <- well_data$RIG_RELEASE_DATE[match(well_results$UWI[(well_results$EUR_exp>0 & !is.na(well_results$EUR_exp))[,]], well_data$X_UWI_DISPLAY[,])]
    if(data_base$vendor =="HPDI") temp <- well_data$X_ONPROD_DATE[match(well_results$UWI[(well_results$EUR_exp>0 & !is.na(well_results$EUR_exp))[,]], well_data$X_UWI_DISPLAY[,])]
    interval_rig_release <- classIntervals(as.numeric(temp), n=num_intervals, style="quantile")  #to define intervals, quantiles or other, over the data range and bin the datapoints into the intervals
    if(data_base$vendor =="PPDM") temp <- well_data$RIG_RELEASE_DATE[match(well_results$UWI[(well_results$EUR_exp>0 & !is.na(well_results$EUR_exp))[,]], well_data$X_UWI_DISPLAY[,])]
    if(data_base$vendor =="HPDI") temp <- well_data$X_ONPROD_DATE[match(well_results$UWI[(well_results$EUR_exp>0 & !is.na(well_results$EUR_exp))[,]], well_data$X_UWI_DISPLAY[,])]
    interval_rig_release <- classIntervals(as.numeric(temp), n=num_intervals, style="fixed", fixedBreaks=interval_rig_release$brks)  #to define intervals, quantiles or other, over the data range and bin the datapoints into the intervals
    pal_rig_release <- findColours(interval_rig_release, pal_blue, under="under", over="over", between="-", digits=0, cutlabels=FALSE)
    zz <- qqnorm(log10(well_results$EUR_exp[(well_results$EUR_exp>0 & !is.na(well_results$EUR_exp))[,]]), datax=FALSE, plot.it=FALSE)
    plot(zz$y, zz$x, main="Log-Normal Q-Q Plot - EURs", pch=16, col=pal_rig_release, xaxt="n", xlim=c(1,4), yaxt="n", ylim=qnorm(c(0.005,0.995), mean = 0, sd = 1), xlab="EUR (MMcf)", ylab="Cumulative Probability")
    ww <- lqs(zz$y, zz$x,method="lms")
    abline(ww, col="Red")
    axis(1,at=(1:4),labels=c("10", "100", "1,000", "10,000"))
    axis.at <- 10^(1:4)
    axis(1, at = log10(1:10 * rep(axis.at[-1] / 10, each = 10)),tcl = -0.5, labels = FALSE,tck=-0.01)
    prob <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.98, 0.99)
    axis(2,at=qnorm(prob, mean = 0, sd = 1),labels=1-prob)
    yy <- qnorm(c(0.1,0.5,0.9), mean = 0, sd = 1)
    xx <- (yy-coefficients(ww)[1])/coefficients(ww)[2]
    text(xx,yy,pos=2,labels=paste(c("P90 = ","P50 = ","P10 = "), formatC(10*round(10^(xx-1)), digits=0, big.mark = ",", format = "d")))
    if(data_base$vendor =="PPDM") temp <- well_data$RIG_RELEASE_DATE[match(well_results$UWI[(well_results$EUR_harm>0 & !is.na(well_results$EUR_harm))[,]], well_data$X_UWI_DISPLAY[,])]
    if(data_base$vendor =="HPDI") temp <- well_data$X_ONPROD_DATE[match(well_results$UWI[(well_results$EUR_harm>0 & !is.na(well_results$EUR_harm))[,]], well_data$X_UWI_DISPLAY[,])]
    interval_rig_release <- classIntervals(as.numeric(temp), n=num_intervals, style="fixed", fixedBreaks=interval_rig_release$brks)  #to define intervals, quantiles or other, over the data range and bin the datapoints into the intervals
    zz <- qqnorm(log10(well_results$EUR_harm[(well_results$EUR_harm>0 & !is.na(well_results$EUR_harm))[,]]), datax=FALSE, plot.it=FALSE)
    points(zz$y, zz$x, pch=16, col=pal_rig_release)
    ww <- lqs(zz$y, zz$x,method="lms")
    abline(ww, col="Red")
    yy <- qnorm(c(0.1,0.5,0.9), mean = 0, sd = 1)
    xx <- (yy-coefficients(ww)[1])/coefficients(ww)[2]
    text(xx,yy,pos=4,labels=paste(c(" P90 = "," P50 = "," P10 = "), formatC(10*round(10^(xx-1)), digits=0, big.mark = ",", format = "d")))
    legend("topleft", title="Rig Release After", as.character(as.POSIXct(interval_rig_release$brks,origin=as.Date("1970-1-1")), format="%b %Y")[1:num_intervals], col=attr(pal_rig_release, "palette"),pch=16,bg="white")
    rm(pal_rig_release,prob,temp)
    rm(ww,xx,yy,zz)
    
    
    # Make of plot of EUR versus maximum calendar day rate
    if(data_base$vendor =="PPDM") temp <- well_data$RIG_RELEASE_DATE[match(well_results$UWI[(well_results$peak_rate>0 & !is.na(well_results$peak_rate) & well_results$EUR_exp>0 & !is.na(well_results$EUR_exp))[,]], well_data$X_UWI_DISPLAY[,])]
    if(data_base$vendor =="HPDI") temp <- well_data$X_ONPROD_DATE[match(well_results$UWI[(well_results$peak_rate>0 & !is.na(well_results$peak_rate) & well_results$EUR_exp>0 & !is.na(well_results$EUR_exp))[,]], well_data$X_UWI_DISPLAY[,])]
    xx <- well_results$peak_rate[(well_results$peak_rate>0 & !is.na(well_results$peak_rate) & well_results$EUR_exp>0 & !is.na(well_results$EUR_exp))[,]]
    yy <- well_results$EUR_exp[(well_results$peak_rate>0 & !is.na(well_results$peak_rate) & well_results$EUR_exp>0 & !is.na(well_results$EUR_exp))[,]]
    interval_rig_release <- classIntervals(as.numeric(temp), n=num_intervals, style="fixed", fixedBreaks=interval_rig_release$brks)  #to define intervals, quantiles or other, over the data range and bin the datapoints into the intervals
    pal_rig_release <- findColours(interval_rig_release, pal_blue, under="under", over="over", between="-", digits=0, cutlabels=FALSE)
    plot(xx, yy, xlim=c(0,4000), ylim=c(0,5000), xlab="Peak Calendar Day Rate (Mcf/d)", ylab="Exponential EUR per Well (MMcf)",main=CapLeading(paste("Group ", formatC(k, digits=2, big.mark = ",", format = "d"), " Wells",sep="")), pch=16, col=pal_rig_release)
    title("Only Wells With More Than 18 Producing Months",line=1,cex.main=0.75)
    best_fit <- lqs(xx,yy,intercept=F)
    abline(best_fit, col="red")
    hght <- strheight("Here")
    text(x=par("usr")[2]/2,y=par("usr")[4]*0.925,paste("Exponential EUR is ", formatC(best_fit$coefficients*100, digits=0, big.mark = ",", format = "d"), "% of the Peak Calendar Day Rate", sep=""))
    legend("bottomright", title="Rig Release After", as.character(as.POSIXct(interval_rig_release$brks,origin=as.Date("1970-1-1")), format="%b %Y")[1:num_intervals], col=attr(pal_rig_release, "palette"),pch=16,bg="white")
    rm(best_fit,hght,pal_rig_release,temp)
    rm(xx,yy)
    
    
    # Make of plot of EUR versus maximum quarterly day rate
    if(data_base$vendor =="PPDM") temp <- well_data$RIG_RELEASE_DATE[match(well_results$UWI[(well_results$peak_quarter>0 & !is.na(well_results$peak_quarter) & well_results$EUR_exp>0 & !is.na(well_results$EUR_exp))[,]], well_data$X_UWI_DISPLAY[,])]
    if(data_base$vendor =="HPDI") temp <- well_data$X_ONPROD_DATE[match(well_results$UWI[(well_results$peak_quarter>0 & !is.na(well_results$peak_quarter) & well_results$EUR_exp>0 & !is.na(well_results$EUR_exp))[,]], well_data$X_UWI_DISPLAY[,])]
    xx <- well_results$peak_quarter[(well_results$peak_quarter>0 & !is.na(well_results$peak_quarter) & well_results$EUR_exp>0 & !is.na(well_results$EUR_exp))[,]]
    yy <- well_results$EUR_exp[(well_results$peak_quarter>0 & !is.na(well_results$peak_quarter) & well_results$EUR_exp>0 & !is.na(well_results$EUR_exp))[,]]
    interval_rig_release <- classIntervals(as.numeric(temp), n=num_intervals, style="fixed", fixedBreaks=interval_rig_release$brks)  #to define intervals, quantiles or other, over the data range and bin the datapoints into the intervals
    pal_rig_release <- findColours(interval_rig_release, pal_blue, under="under", over="over", between="-", digits=0, cutlabels=FALSE)
    plot(xx, yy, xlim=c(0,4000), ylim=c(0,5000), xlab="Peak Quarterly Rate (Mcf/d)", ylab="Exponential EUR per Well (MMcf)",main=CapLeading(paste("Group ", formatC(k, digits=2, big.mark = ",", format = "d"), " Wells",sep="")), pch=16, col=pal_rig_release)
    title("Only Wells With More Than 18 Producing Months",line=1,cex.main=0.75)
    best_fit <- lqs(xx,yy,intercept=F)
    abline(best_fit, col="red")
    hght <- strheight("Here")
    text(x=par("usr")[2]/2,y=par("usr")[4]*0.925,paste("Exponential EUR is ", formatC(best_fit$coefficients*100, digits=0, big.mark = ",", format = "d"), "% of the Peak Quarterly Rate", sep=""))
    legend("bottomright", title="Rig Release After", as.character(as.POSIXct(interval_rig_release$brks,origin=as.Date("1970-1-1")), format="%b %Y")[1:num_intervals], col=attr(pal_rig_release, "palette"),pch=16,bg="white")
    rm(best_fit,hght,pal_rig_release,temp)
    rm(xx,yy)
    
    
    # Make of plot of EUR versus maximum calendar day rate
    if(data_base$vendor =="PPDM") temp <- well_data$RIG_RELEASE_DATE[match(well_results$UWI[(well_results$peak_rate>0 & !is.na(well_results$peak_rate) & well_results$EUR_harm>0 & !is.na(well_results$EUR_harm))[,]], well_data$X_UWI_DISPLAY[,])]
    if(data_base$vendor =="HPDI") temp <- well_data$X_ONPROD_DATE[match(well_results$UWI[(well_results$peak_rate>0 & !is.na(well_results$peak_rate) & well_results$EUR_harm>0 & !is.na(well_results$EUR_harm))[,]], well_data$X_UWI_DISPLAY[,])]
    xx <- well_results$peak_rate[(well_results$peak_rate>0 & !is.na(well_results$peak_rate) & well_results$EUR_harm>0 & !is.na(well_results$EUR_harm))[,]]
    yy <- well_results$EUR_harm[(well_results$peak_rate>0 & !is.na(well_results$peak_rate) & well_results$EUR_harm>0 & !is.na(well_results$EUR_harm))[,]]
    interval_rig_release <- classIntervals(as.numeric(temp), n=num_intervals, style="fixed", fixedBreaks=interval_rig_release$brks)  #to define intervals, quantiles or other, over the data range and bin the datapoints into the intervals
    pal_rig_release <- findColours(interval_rig_release, pal_blue, under="under", over="over", between="-", digits=0, cutlabels=FALSE)
    plot(xx, yy, xlim=c(0,4000), ylim=c(0,5000), xlab="Peak Calendar Day Rate (Mcf/d)", ylab="Harmonic EUR per Well (MMcf)",main=CapLeading(paste("Group ", formatC(k, digits=2, big.mark = ",", format = "d"), " Wells",sep="")), pch=16, col=pal_rig_release)
    title("Only Wells With More Than 18 Producing Months",line=1,cex.main=0.75)
    best_fit <- lqs(xx,yy,intercept=F)
    abline(best_fit, col="red")
    hght <- strheight("Here")
    text(x=par("usr")[2]/2,y=par("usr")[4]*0.925,paste("Harmonic EUR is ", formatC(best_fit$coefficients*100, digits=0, big.mark = ",", format = "d"), "% of the Peak Calendar Day Rate", sep=""))
    legend("bottomright", title="Rig Release After", as.character(as.POSIXct(interval_rig_release$brks,origin=as.Date("1970-1-1")), format="%b %Y")[1:num_intervals], col=attr(pal_rig_release, "palette"),pch=16,bg="white")
    rm(best_fit,hght,pal_rig_release,temp)
    rm(xx,yy)
    
    
    
    # Make of plot of EUR versus maximum quarterly day rate
    if(data_base$vendor =="PPDM") temp <- well_data$RIG_RELEASE_DATE[match(well_results$UWI[(well_results$peak_quarter>0 & !is.na(well_results$peak_quarter) & well_results$EUR_harm>0 & !is.na(well_results$EUR_harm))[,]], well_data$X_UWI_DISPLAY[,])]
    if(data_base$vendor =="HPDI") temp <- well_data$X_ONPROD_DATE[match(well_results$UWI[(well_results$peak_quarter>0 & !is.na(well_results$peak_quarter) & well_results$EUR_harm>0 & !is.na(well_results$EUR_harm))[,]], well_data$X_UWI_DISPLAY[,])]
    xx <- well_results$peak_quarter[(well_results$peak_quarter>0 & !is.na(well_results$peak_quarter) & well_results$EUR_harm>0 & !is.na(well_results$EUR_harm))[,]]
    yy <- well_results$EUR_harm[(well_results$peak_quarter>0 & !is.na(well_results$peak_quarter) & well_results$EUR_harm>0 & !is.na(well_results$EUR_harm))[,]]
    interval_rig_release <- classIntervals(as.numeric(temp), n=num_intervals, style="fixed", fixedBreaks=interval_rig_release$brks)  #to define intervals, quantiles or other, over the data range and bin the datapoints into the intervals
    pal_rig_release <- findColours(interval_rig_release, pal_blue, under="under", over="over", between="-", digits=0, cutlabels=FALSE)
    plot(xx, yy, xlim=c(0,4000), ylim=c(0,5000), xlab="Peak Quarterly Rate (Mcf/d)", ylab="Harmonic EUR per Well (MMcf)",main=CapLeading(paste("Group ", formatC(k, digits=2, big.mark = ",", format = "d"), " Wells",sep="")), pch=16, col=pal_rig_release)
    title("Only Wells With More Than 18 Producing Months",line=1,cex.main=0.75)
    best_fit <- lqs(xx,yy,intercept=F)
    abline(best_fit, col="red")
    hght <- strheight("Here")
    text(x=par("usr")[2]/2,y=par("usr")[4]*0.925,paste("Harmonic EUR is ", formatC(best_fit$coefficients*100, digits=0, big.mark = ",", format = "d"), "% of the Peak Quarterly Rate", sep=""))
    legend("bottomright", title="Rig Release After", as.character(as.POSIXct(interval_rig_release$brks,origin=as.Date("1970-1-1")), format="%b %Y")[1:num_intervals], col=attr(pal_rig_release, "palette"),pch=16,bg="white")
    rm(best_fit,hght,pal_rig_release,temp)
    rm(xx,yy)
    
    
    title(main=paste("EUR Correlations for Group ", formatC(k, digits=2, big.mark = ",", format = "d"), " Wells",sep=""),outer=TRUE)
    dev.off() # EUR Correlations
    
    rm(interval_rig_release)
    
    write.csv(well_results, file=paste("Group ", formatC(k, digits=2, big.mark = ",", format = "d")," Results.csv",sep=""),row.names=FALSE)
    write.csv(merge(well_data, well_results, by.x = "X_UWI_DISPLAY", by.y = "UWI", all.x=FALSE, all.y=FALSE, trace = TRUE), file=paste("Group ", formatC(k, digits=2, big.mark = ",", format = "d")," Data and Results.csv",sep=""),row.names=FALSE)
    
    
    #delete(well_data,well_results)
    rm(well_data,well_results,prob,axis.at)
    gc()
    
  } #k means stuff
  UpdateLocationsMongoWellCollection(mongo_data_base, origproj="+proj=longlat +datum=NAD27", newproj="+proj=longlat +datum=WGS84", CPU_count=CPU_count)
  ForceNormalizationUpdateUWIs <- Arps_Forecasts(mongo_data_base, CPU_count=CPU_count)
  if(length(ForceNormalizationUpdateUWIs)>0) {
    Normalize_Decline(data_base, mongo_data_base, CPU_count, ResourcePlay=NA_real_, ForceUWI=ForceNormalizationUpdateUWIs, limit=length(ForceNormalizationUpdateUWIs))
  }
  rm(ForceNormalizationUpdateUWIs)
  Normalize_Decline(data_base, mongo_data_base, CPU_count, ResourcePlay, ForceUWI=NA_real_, limit=5000)
  UpdateMongoMajorProduct(mongo_data_base, ResourcePlay, CPU_count)
}	# end of the for loop that processes each group
UpdateMongoWellCollection(mongo_data_base, ResourcePlay, CPU_count, PurgeWellCollection=TRUE)
#UpdateMongoRecentCollection(mongo_data_base, ResourcePlay, CPU_count, PurgeRecentCollection=TRUE)
#Update_Cardinality(data_base, mongo_data_base, ResourcePlay=ResourcePlay, CPU_count=CPU_count, ForceUpdate=TRUE)
Update_Cardinality(data_base, mongo_data_base, ResourcePlay=ResourcePlay, CPU_count=CPU_count, ForceUpdate=FALSE)
#Update_P10P90Ratio(mongo_data_base, ResourcePlay, CPU_count, ForceUpdate=TRUE)
Update_P10P90Ratio(mongo_data_base, ResourcePlay, CPU_count, ForceUpdate=FALSE)
UpdateMongoMajorProduct(mongo_data_base, ResourcePlay=NA_real_, CPU_count)








if(mongodb_save) {
  
  #list_json_files <- list.files(pattern="[.]json$", path=paste(workdir, "json", sep=""), full.names=TRUE)
  list_json_files <- list.files(pattern="[.]json$", path=paste(workdir), full.names=TRUE)
  
  sfInit(parallel=TRUE, cpus=CPU_count, type="SOCK", socketHosts=rep('localhost',CPU_count) )
  sfExport("list_json_files","mongo_data_base","WriteDeclineMongo")
  sfLibrary(jsonlite)
  sfLibrary(mongolite)
  sfLibrary(base)
  temp <- lapply(1:length(list_json_files), function(i) {
    #temp <- lapply(1:1, function(i) {
    sfLapply(
      as.list(readLines(list_json_files[i])), 
      WriteJSONDeclineMongo
    )
    file.move(list_json_files[i], paste(workdir, "json/", sep=""))
  }
  )
  sfStop()
  gc()
  #WriteDeclineMongo(x=as.list(readLines(list_json_files[10]))[1])
  
  rm(list_json_files)
  
}


if(calculate_stan) {
  
  library(rstan)
  #library(parallel)
  #source("http://mc-stan.org/rstan/stan.R")
  rstan_options(auto_write=TRUE)
  options(mc.cores=1)
  #options(mc.cores=parallel::detectCores())
  
  
  if(FALSE) {
    for(i in 1:length(x)) {
      if(length(subset(x[[i]], q>0)$q)>36) {
        plot(log(q)~t,data=x[[i]],main=i)
        stopnow <- readline()
        if(stopnow=="y") break
      }
    }
  }
  
  i<-15
  temp <- subset(x[[i]],q>0)
  temp$comp_t <- cumsum(as.numeric(days_in_month(temp$t)))
  StanProduction <- list(
    N=nrow(temp),
    t=temp$comp_t,
    q=temp$q,
    Q=temp$Q,
    maxrate=max(temp$q),
    cumtodate=max(temp$Q),
    qf=75
  )
  #plot(q~t, data=x[[i]],main=i)
  plot(q~Q, data=x[[i]],main=i)
  #plot(q~t, ylim=c(0,100), data=x[[i]],main=i)
  #plot(q~Q, ylim=c(0,100), xlim=c(0,1e5), data=x[[i]],main=i)
  
  
  StanSimpleDeclineResults <- stan(
    model_code = StanSimpleDeclineModel,
    data = StanProduction,
    iter = iters
  )
  hist(extract(SimpleDeclineResults)$tBDF,breaks=(0:100)/10)
  
  StanArpsResults <- stan(
    model_code = StanArpsModel,
    data = StanProduction,
    iter = iters
  )
  hist(extract(ArpsResults)$EUR,breaks=100)
  hist(extract(ArpsResults)$b,breaks=100)
  hist(extract(ArpsResults)$Di,breaks=100)
  hist(extract(ArpsResults)$qi,breaks=100)
  
  cl <- makeCluster(rep('localhost',CPU_count), type = "SOCK")
  registerDoSNOW(cl)
  #clusterExport(cl,c("Abandon_Rate_Gas","Abandon_Rate_Oil")) 
  #if(products[j]=="Gas") { 
  #  x <- dlply(subset(prod_data,PROD_DATE<=dates[timestep])[,], .(X_UWI_DISPLAY), Gas_Data, .parallel = TRUE)
  #}
  #if(products[j]=="Oil") {  
  #  x <- dlply(subset(prod_data,PROD_DATE<=dates[timestep])[,], .(X_UWI_DISPLAY), Oil_Data, .parallel = TRUE)
  #}
  stopCluster(cl)
  rm(cl)
  
  
  StanDuongResults <- stan(
    model_code = StanDuongModel,
    data = StanProduction,
    iter = iters
  )
  hist(extract(DuongResults)$a,breaks=100)
  hist(extract(DuongResults)$qi,breaks=100)
  hist(extract(DuongResults)$m,breaks=100)
  hist(extract(DuongResults)$qinf,breaks=100)
  
  
  StanSepdResults <- stan(
    model_code = StanSepdModel,
    data = StanProduction,
    iter = iters
  )
  #hist(extract(SepdResults)$tau,breaks=100)
  hist(extract(SepdResults)$EUR,breaks=100)
  
  
  StanPleResults <- stan(
    model_code = StanPleModel,
    data = StanProduction,
    iter = iters
  )
  hist(extract(PleResults)$b,breaks=100)
  
  
  time <- Sys.time()
  StanLgmResults <- stan(
    model_code = StanLgmModel,
    data = StanProduction,
    iter = iters
  )
  print(Sys.time()-time)
  hist(extract(LgmResults)$K,breaks=100)
  
  
  StanTwoStageArpsResults <- stan(
    model_code = StanTwoStageArpsModel,
    data = StanProduction,
    iter = iters
  )
  hist(extract(TwoStageArpsResults)$tBDF,breaks=100)
  hist(extract(TwoStageArpsResults)$q1,breaks=100)
  hist(extract(TwoStageArpsResults)$q2,breaks=100)
  hist(extract(TwoStageArpsResults)$b1,breaks=100)
  hist(extract(TwoStageArpsResults)$b2,breaks=100)
  
  
}


write.csv(group_results, file=paste("Group Results.csv",sep=""),row.names=FALSE)
write.csv(group_results, file=paste("Group Results no NA.csv",sep=""), na="",row.names=FALSE)
write.csv(group_seg_results, file=paste("Group Seg Results.csv",sep=""),row.names=FALSE)
write.csv(group_seg_results, file=paste("Group Seg Results no NA.csv",sep=""), na="",row.names=FALSE)
#plot(group_results$peak_rate[,][!is.na(group_results$EUR_exp[,]) & !is.na(group_results$peak_rate[,])], group_results$EUR_exp[!is.na(group_results$EUR_exp[,]) & !is.na(group_results$peak_rate[,])],log="xy")
#odbcCloseAll()
#rm(num_intervals)





if(FALSE) {
  
  library(rstan)
  #source("http://mc-stan.org/rstan/stan.R")
  rstan_options(auto_write=TRUE)
  options(mc.cores=1)
  #options(mc.cores=parallel::detectCores())
  
  
  if(FALSE) {
    for(i in 1:length(x)) {
      if(length(subset(x[[i]], q>0)$q)>36) {
        plot(log(q)~t,data=x[[i]],main=i)
        stopnow <- readline()
        if(stopnow=="y") break
      }
    }
  }
  
  i<-15
  temp <- subset(x[[i]],q>0)
  temp$comp_t <- cumsum(as.numeric(days_in_month(temp$t)))
  StanProduction <- list(
    N=nrow(temp),
    t=temp$comp_t,
    q=temp$q,
    Q=temp$Q,
    maxrate=max(temp$q),
    cumtodate=max(temp$Q),
    qf=75
  )
  #plot(q~t, data=x[[i]],main=i)
  plot(q~Q, data=x[[i]],main=i)
  #plot(q~t, ylim=c(0,100), data=x[[i]],main=i)
  #plot(q~Q, ylim=c(0,100), xlim=c(0,1e5), data=x[[i]],main=i)
  
  SimpleDeclineModel <- "
  data {
  int <lower=1> N;
  vector[N] t;
  vector[N] q;
  vector[N] Q;
  real cumtodate;
  real qf;
  }
  parameters {
  real Beta[2];
  real Alpha;
  real <lower=0> s2;
  real <lower=0> tBDF;
  }
  transformed parameters {
  vector[N] yhat;
  for (i in 1:N)
  yhat[i] <- Alpha+(log(t[i])-tBDF)*Beta[1+(log(t[i])>tBDF)];
  }
  model {
  log(q) ~ normal(yhat,sqrt(s2));
  s2 ~ uniform(0,1e3);
  tBDF ~ uniform(0,10);
  Alpha ~ normal(0,1000);
  Beta ~ normal(0,1000);
  }
  "
  SimpleDeclineResults <- stan(
    model_code = SimpleDeclineModel,
    data = StanProduction,
    iter = 2000,
    pars=c("Beta","Alpha","s2","tBDF")
  )
  hist(extract(SimpleDeclineResults)$tBDF,breaks=(0:100)/10)
  
  
  DuongModel <- "
  data {
  int <lower=1> N;
  vector[N] t;
  vector[N] q;
  vector[N] Q;
  real <lower=0> maxrate;
  real <lower=0> cumtodate;
  real <lower=0> qf;
  }
  parameters {
  real <lower=0> qi;
  real <lower=0> a;
  real <lower=0> m;
  real qinf;
  real <lower=0> s2;
  //real <lower=0> s3;
  }
  transformed parameters {
  vector[N] yhat;
  vector[N] tam;
  //vector[N] Qhat;
  for (i in 1:N) {
  tam[i] <- (t[i]^(-m))*exp((a/(1.0-m))*((t[i]^(1.0-m))-1.0));
  //yhat[i] <- qi*tam[i];
  yhat[i] <- qi*tam[i] + qinf;
  //Qhat[i] <- qi*tam[i]/(a*t[i]^(-m));
  }
  }
  model {
  qi ~ uniform(0,2*maxrate);
  //qi ~ normal(maxrate,sqrt(maxrate));
  a ~ uniform(0,20);
  m ~ uniform(0,10);
  qinf ~ uniform(0,qf);
  s2 ~ uniform(0,1e3);
  //s3 ~ uniform(0,1e3);
  q ~ normal(yhat,sqrt(s2));
  //Q ~ normal(Qhat,sqrt(s3));
  }
  generated quantities {
  //  real EUR;
  //  EUR <- fmax(cumtodate, ((qi^b)/(Di*(1.0-b)))*(qi^(1.0-b)-qf^(1.0-b)));
  }
  "
  
  DuongResults <- stan(
    model_code = DuongModel,
    data = StanProduction,
    iter = 2000
  )
  hist(extract(DuongResults)$a,breaks=100)
  hist(extract(DuongResults)$qi,breaks=100)
  hist(extract(DuongResults)$m,breaks=100)
  hist(extract(DuongResults)$qinf,breaks=100)
  
  
  SepdModel <- "
  data {
  int <lower=1> N;
  vector[N] t;
  vector[N] q;
  vector[N] Q;
  real <lower=0> cumtodate;
  real <lower=0> maxrate;
  real <lower=0> qf;
  }
  parameters {
  real <lower=0> qi;
  real <lower=0> tau;
  real <lower=0,upper=4> n;
  real <lower=0> s2;
  }
  transformed parameters {
  vector[N] qhat;
  //vector[N] Qhat;
  for (i in 1:N) {
  qhat[i] <- qi*exp(-(t[i]/tau)^n);
  //  Qhat[i] <- (qi*tau/n)*(tgamma(1.0/n)-tgamma(1.0/n,(t[i]/tau)^n));
  }
  }
  model {
  qi ~ uniform(0,2*maxrate);
  tau ~ uniform(0,5000);
  n ~ uniform(0,4);
  s2 ~ uniform(0,1e3);
  q ~ normal(qhat,sqrt(s2));
  }
  generated quantities {
  real EUR;
  EUR <- ((qi^tau)/n)*tgamma(1.0/n);
  }
  "
  
  SepdResults <- stan(
    model_code = SepdModel,
    data = StanProduction,
    iter = 20000
  )
  #hist(extract(SepdResults)$tau,breaks=100)
  hist(extract(SepdResults)$EUR,breaks=100)
  
  
  PleModel <- "
  data {
  int <lower=1> N;
  vector[N] t;
  vector[N] q;
  real <lower=0> cumtodate;
  real <lower=0> maxrate;
  real <lower=0> qf;
  }
  parameters {
  real <lower=0> qi;
  real <lower=0> Di;
  real <lower=0,upper=4> n;
  real <lower=0> Dinf;
  real <lower=0> s2;
  }
  transformed parameters {
  vector[N] yhat;
  for (i in 1:N)
  yhat[i] <- qi*exp(-Di*(t[i]^n) - Dinf*t[i]);
  }
  model {
  qi ~ uniform(0,2*maxrate);
  Di ~ uniform(0,10);
  Dinf ~ uniform(0,10);
  n ~ uniform(0,4);
  s2 ~ uniform(0,1e3);
  q ~ normal(yhat,sqrt(s2));
  }
  generated quantities {
  real b;
  b <- -(Di*(n-1)*t[N]^n)/((Dinf*t[N] + Di*t[N]^n)^2);
  }
  "
  
  PleResults <- stan(
    model_code = PleModel,
    data = StanProduction,
    iter = 2000
  )
  hist(extract(PleResults)$b,breaks=100)
  
  
  LgmModel <- "
  data {
  int <lower=1> N;
  vector[N] t;
  vector[N] q;
  vector[N] Q;
  real <lower=0> cumtodate;
  real <lower=0> qf;
  }
  parameters {
  real <lower=0> a;
  real <lower=0> n;
  real <lower=0> K;
  real <lower=0> s2;
  real <lower=0> s3;
  }
  transformed parameters {
  vector[N] yhat;
  vector[N] Qhat;
  for (i in 1:N) {
  yhat[i] <- (K*n*a*t[i]^(n-1.0))/((a+t[i]^n)^2);
  Qhat[i] <- (K*t[i]^n)/(a+t[i]^n);
  }
  }
  model {
  a ~ uniform(0,1000);
  n ~ uniform(0,10);
  K ~ uniform(0,1e5);
  s2 ~ uniform(0,1e3);
  s3 ~ uniform(0,1e3);
  q ~ normal(yhat,sqrt(s2));
  Q ~ normal(Qhat,sqrt(s3));
  }
  generated quantities {
  //  real EUR;
  //  EUR <- fmax(cumtodate, ((qi^b)/(Di*(1.0-b)))*(qi^(1.0-b)-qf^(1.0-b)));
  }
  "  
  
  time <- Sys.time()
  LgmResults <- stan(
    model_code = LgmModel,
    data = StanProduction,
    iter = 2000
  )
  print(Sys.time()-time)
  hist(extract(LgmResults)$K,breaks=100)
  
  
  TwoStageArpsModel <- "
  data {
  int <lower=1> N;
  vector[N] t;
  vector[N] q;
  vector[N] Q;
  real <lower=0> cumtodate;
  real <lower=0> maxrate;
  real <lower=0> qf;
  }
  parameters {
  real <lower=0> q1;
  real <lower=0, upper=q1> q2;
  real D1;
  real D2;
  real <lower=0,upper=4> b1;
  real <lower=0,upper=b1> b2;
  real <lower=100> tBDF;
  real <lower=0> s2;
  //real <lower=0> s3;
  }
  transformed parameters {
  vector[N] yhat;
  //vector[N] Qhat;
  for (i in 1:N) {
  if (t[i]<tBDF) {
  yhat[i] <- q1/((1.0 + b1*D1*t[i])^(inv(b1)));
  //  Qhat[i] <- ((q1^b1)/(D1*(1.0-b1)))*(q1^(1.0-b1)-q[i]^(1.0-b1));
  }
  else {
  yhat[i] <- q2/((1.0 + b2*D2*t[i])^(inv(b2)));
  //  Qhat[i] <- ((q2^b2)/(D2*(1.0-b2)))*(q2^(1.0-b2)-q[i]^(1.0-b2));
  }
  }
  }
  model {
  q1 ~ uniform(0,2*maxrate);
  q2 ~ uniform(0,2*maxrate);
  D1 ~ uniform(0,10);
  D2 ~ uniform(0,10);
  b1 ~ uniform(0,4);
  b2 ~ uniform(0,4);
  s2 ~ uniform(0,1e3);
  //s3 ~ uniform(0,1e3);
  tBDF ~ uniform(0,3650);
  q ~ normal(yhat,sqrt(s2));
  //Q ~ normal(Qhat,sqrt(s3));
  }
  generated quantities {
  //  real EUR;
  //  EUR <- fmax(cumtodate, ((qi^b)/(Di*(1.0-b)))*(qi^(1.0-b)-qf^(1.0-b)));
  }
  "
  
  TwoStageArpsResults <- stan(
    model_code = TwoStageArpsModel,
    data = StanProduction,
    iter = 2000,
    pars=c("q1","D1","b1","q2","D2","b2","s2","tBDF")
  )
  hist(extract(TwoStageArpsResults)$tBDF,breaks=100)
  hist(extract(TwoStageArpsResults)$q1,breaks=100)
  hist(extract(TwoStageArpsResults)$q2,breaks=100)
  hist(extract(TwoStageArpsResults)$b1,breaks=100)
  hist(extract(TwoStageArpsResults)$b2,breaks=100)
  
  
}


write.csv(group_results, file=paste("Group Results.csv",sep=""),row.names=FALSE)
write.csv(group_results, file=paste("Group Results no NA.csv",sep=""), na="",row.names=FALSE)
write.csv(group_seg_results, file=paste("Group Seg Results.csv",sep=""),row.names=FALSE)
write.csv(group_seg_results, file=paste("Group Seg Results no NA.csv",sep=""), na="",row.names=FALSE)
#plot(group_results$peak_rate[,][!is.na(group_results$EUR_exp[,]) & !is.na(group_results$peak_rate[,])], group_results$EUR_exp[!is.na(group_results$EUR_exp[,]) & !is.na(group_results$peak_rate[,])],log="xy")
#odbcCloseAll()
#rm(num_intervals)

