##############################################################
# Load all required packages and define our global variables #
##############################################################

#require(hexbin)
require(ggplot2)
require(scales)
require(ffbase)
require(plotKML)
require(rgdal)
require(gstat)
#require(SDMTools)
require(classInt)
require(RColorBrewer)
require(MASS)
require(maps)
require(mapdata)
require(maptools)
require(colorspace)
require(plotGoogleMaps)




####################################################
# Load our data and do basic data.table transforms #
####################################################

results <- read.csv.ffdf(file="PPDM_Results_Third_Pass.csv", header=TRUE, VERBOSE=TRUE, first.rows=10000, next.rows=50000, colClasses=NA)
results2 <- read.csv.ffdf(file="HPDI_Results_Third_Pass.csv", header=TRUE, VERBOSE=TRUE, first.rows=10000, next.rows=50000, colClasses=NA)
results3 <- ffdfappend(results, results2)
results_mon <- read.csv.ffdf(file="Group Results Montney no NA.csv", header=TRUE, VERBOSE=TRUE, first.rows=10000, next.rows=50000, colClasses=NA)



####################
# Analyse the data #
####################

myBreaks <- 10^seq(0:5)
dat <- data.frame(x=results$peak_rate[!is.na(results$peak_rate[,]) & !is.na(results$EUR_exp[,])],y=results$EUR_exp[!is.na(results$peak_rate[,]) & !is.na(results$EUR_exp[,])])
p <- qplot(x, y, data = dat, log="xy", geom="hex", xlab="Peak Monthly Gas Rate (Mcf/d)", ylab="EUR from Exponential Decline (MMcf)",main="EUR of Every Gas Well in Western Canada") + scale_x_log10(limits = c(10^-1, 10^9), breaks = 10^seq(0, 9, by = 2), labels = trans_format("log10", math_format(10^.x))) + scale_y_log10(limits = c(10^-3, 10^7), breaks = 10^seq(-2, 6, by = 2), labels = trans_format("log10", math_format(10^.x)))
p + scale_fill_gradient(low = "white", high = "steelblue", name = "count", trans = "log", breaks = myBreaks, labels = myBreaks) + theme(legend.justification=c(1,0), legend.position=c(1,0))

pdf("PPDM_Hexplot.pdf", width = 11, height = 8.5) #for memo
print(p + scale_fill_gradient(low = "white", high = "steelblue", name = "count", trans = "log", breaks = myBreaks, labels = myBreaks) + theme(legend.justification=c(1,0), legend.position=c(1,0)))
dev.off()


myBreaks <- 10^seq(0:5)
dat <- data.frame(x=results2$peak_rate[!is.na(results2$peak_rate[,]) & !is.na(results2$EUR_exp[,]) & results2$peak_rate[,]>0 & results2$EUR_exp[,]>0],y=results2$EUR_exp[!is.na(results2$peak_rate[,]) & !is.na(results2$EUR_exp[,]) & results2$peak_rate[,]>0 & results2$EUR_exp[,]>0])
p <- qplot(x, y, data = dat, log="xy", geom="hex", xlab="Peak Monthly Gas Rate (Mcf/d)", ylab="EUR from Exponential Decline (MMcf)",main="EUR of Every Gas Well in US") + scale_x_log10(limits = c(10^-1, 10^9), breaks = 10^seq(0, 9, by = 2), labels = trans_format("log10", math_format(10^.x))) + scale_y_log10(limits = c(10^-3, 10^7), breaks = 10^seq(-2, 6, by = 2), labels = trans_format("log10", math_format(10^.x)))
p + scale_fill_gradient(low = "white", high = "steelblue", name = "count", trans = "log", breaks = myBreaks, labels = myBreaks) + theme(legend.justification=c(1,0), legend.position=c(1,0))

pdf("HPDI_Hexplot.pdf", width = 11, height = 8.5) #for memo
print(p + scale_fill_gradient(low = "white", high = "steelblue", name = "count", trans = "log", breaks = myBreaks, labels = myBreaks) + theme(legend.justification=c(1,0), legend.position=c(1,0)))
dev.off()


myBreaks <- 10^seq(0:5)
dat <- data.frame(x=results3$peak_rate[!is.na(results3$peak_rate[,]) & !is.na(results3$EUR_exp[,]) & results3$peak_rate[,]>0 & results3$EUR_exp[,]>0],y=results3$EUR_exp[!is.na(results3$peak_rate[,]) & !is.na(results3$EUR_exp[,]) & results3$peak_rate[,]>0 & results3$EUR_exp[,]>0])
p <- qplot(x, y, data = dat, log="xy", geom="hex", xlab="Peak Monthly Gas Rate (Mcf/d)", ylab="EUR from Exponential Decline (MMcf)",main="EUR of Every Gas Well in N America") + scale_x_log10(limits = c(10^-1, 10^9), breaks = 10^seq(0, 9, by = 2), labels = trans_format("log10", math_format(10^.x))) + scale_y_log10(limits = c(10^-3, 10^7), breaks = 10^seq(-2, 6, by = 2), labels = trans_format("log10", math_format(10^.x)))
p + scale_fill_gradient(low = "white", high = "steelblue", name = "count", trans = "log", breaks = myBreaks, labels = myBreaks) + theme(legend.justification=c(1,0), legend.position=c(1,0))


pdf("N_America_Hexplot.pdf", width = 11, height = 8.5) #for memo
print(p + scale_fill_gradient(low = "white", high = "steelblue", name = "count", trans = "log", breaks = myBreaks, labels = myBreaks) + theme(legend.justification=c(1,0), legend.position=c(1,0)))
dev.off()

png("N_America_Hexplot.png", width = 5000, height = 4000,res=600) #single plot
print(p + scale_fill_gradient(low = "white", high = "steelblue", name = "count", trans = "log", breaks = myBreaks, labels = myBreaks) + theme(legend.justification=c(1,0), legend.position=c(1,0)))
dev.off()


png("N_America_500Mcf_Hist.png", width = 5000, height = 4000,res=600) #single plot
dat <- data.frame(x=results3$peak_rate[!is.na(results3$peak_rate[,]) & !is.na(results3$EUR_exp[,]) & results3$peak_rate[,]>0 & results3$EUR_exp[,]>0],y=results3$EUR_exp[!is.na(results3$peak_rate[,]) & !is.na(results3$EUR_exp[,]) & results3$peak_rate[,]>0 & results3$EUR_exp[,]>0])
dat_exp <- dat[dat$x>400 & dat$x<600,]
hist_exp <- hist(dat_exp$y, n=10000)
plot(hist_exp$mids,hist_exp$density, log="x", xlim=c(1,10000), xlab="EUR (MMcf)", ylab="Density", main="Histogram of EURs for 500 Mcf/d Wells", col="#2222dd55", pch=16, cex=0.5)
lines(density(dat_exp$y), col="blue", lwd=3)
dev.off()


rm(dat,dat_exp,hist_exp,myBreaks,p)




png("N_America_Declines.png", width = 5000, height = 4000,res=600) #single plot
temp <- results3$X_ONPROD_DATE[,][!is.na(results3$D_1[,])]
interval_rig_release <- classIntervals(as.numeric(as.POSIXct(levels(temp))), n=num_intervals, style="quantile")
pal_rig_release <- findColours(interval_rig_release, pal_accent, under="under", over="over", between="-", digits=0, cutlabels=FALSE)
zz <- qqnorm(log10(100*results3$D_1[,][!is.na(results3$D_1[,]) & results3$D_1[,]>0]), datax=FALSE, plot.it=FALSE)
plot(zz$y, zz$x, main="Log-Normal Q-Q Plot - Decline Rates\n(at end of 1,2,3,5,10,20,30 years)", pch=16, col=alpha(pal_rig_release,0.95), xaxt="n", xlim=c(0,3), yaxt="n", ylim=qnorm(c(0.005,0.995), mean = 0, sd = 1), xlab="Decline (%)", ylab="Cumulative Probability",xaxs="i",yaxs="i")
ww <- lmsreg(zz$y, zz$x)
abline(ww, col="Red")
axis(1,at=(0:3),labels=c("1.0", "10.0", "100.0", "1000.0"))
axis.at <- 10^(0:3)
axis(1, at = log10(1:10 * rep(axis.at[-1] / 10, each = 10)),tcl = -0.5, labels = FALSE,tck=-0.01)
prob <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.98, 0.99)
axis(2,at=qnorm(prob, mean = 0, sd = 1),labels=FALSE)
#axis(2,at=qnorm(prob, mean = 0, sd = 1),labels=1-prob)
axis(2,at=qnorm(prob[c(-1, -5, -11, -15)], mean = 0, sd = 1),labels=1-prob[c(-1, -5, -11, -15)])
yy <- qnorm(c(0.1,0.5,0.9), mean = 0, sd = 1)
xx <- (yy-coefficients(ww)[1])/coefficients(ww)[2]
text(xx,yy,pos=4,labels=paste(c(" P90 = "," P50 = "," P10 = "), formatC(round(10^xx), digits=0, big.mark = ",", format = "d"),c("%","%","%"),sep=""))
yy <- qnorm(c(0.3), mean = 0, sd = 1)
xx <- (yy-coefficients(ww)[1])/coefficients(ww)[2]
text(xx,yy,pos=4,labels=" Decline Rate at \n End of 1 Year")
legend("topleft", title="Rig Release After", as.character(as.POSIXct(interval_rig_release$brks,origin=as.Date("1970-1-1")), format="%b %Y")[1:num_intervals], col=attr(pal_rig_release, "palette"),pch=16,bg="white")
# Think twice, or five times,  about changing the next line... possible values of "i" are hardcoded in several spots
for(i in c(2,3,5,10,20,30)) {  
if(i==2) temp <- results3$X_ONPROD_DATE[,][!is.na(results3$D_2[,])]
if(i==3) temp <- results3$X_ONPROD_DATE[,][!is.na(results3$D_3[,])]
if(i==5) temp <- results3$X_ONPROD_DATE[,][!is.na(results3$D_5[,])]
if(i==10) temp <- results3$X_ONPROD_DATE[,][!is.na(results3$D_10[,])]
if(i==20) temp <- results3$X_ONPROD_DATE[,][!is.na(results3$D_20[,])]
if(i==30) temp <- results3$X_ONPROD_DATE[,][!is.na(results3$D_30[,])]
if(length(temp)>5){
  interval_rig_release <- classIntervals(as.numeric(as.POSIXct(levels(temp))), n=num_intervals, style="fixed", fixedBreaks=interval_rig_release$brks)  #to define intervals, quantiles or other, over the data range and bin the datapoints into the intervals
  pal_rig_release <- findColours(interval_rig_release, pal_accent, under="under", over="over", between="-", digits=0, cutlabels=FALSE)
  if(i==2) zz <- qqnorm(log10(100*results3$D_2[,][!is.na(results3$D_2[,]) & results3$D_2[,]>0]), datax=FALSE, plot.it=FALSE)
  if(i==3) zz <- qqnorm(log10(100*results3$D_3[,][!is.na(results3$D_3[,]) & results3$D_3[,]>0]), datax=FALSE, plot.it=FALSE)
  if(i==5) zz <- qqnorm(log10(100*results3$D_5[,][!is.na(results3$D_5[,]) & results3$D_5[,]>0]), datax=FALSE, plot.it=FALSE)
  if(i==10) zz <- qqnorm(log10(100*results3$D_10[,][!is.na(results3$D_10[,]) & results3$D_10[,]>0]), datax=FALSE, plot.it=FALSE)
  if(i==20) zz <- qqnorm(log10(100*results3$D_20[,][!is.na(results3$D_20[,]) & results3$D_20[,]>0]), datax=FALSE, plot.it=FALSE)
  if(i==30) zz <- qqnorm(log10(100*results3$D_30[,][!is.na(results3$D_30[,]) & results3$D_30[,]>0]), datax=FALSE, plot.it=FALSE)
  points(zz$y, zz$x, pch=16, col=alpha(pal_rig_release,0.95))
  ww <- lmsreg(zz$y, zz$x)
  abline(ww, col="Red")
} # end for loop that checks for enough data points
} # end for loop for decline rate to use
dev.off()









#http://gsif.isric.org/doku.php?id=wiki:tutorial_plotkml


wells_points <- SpatialPoints(cbind(results3$SURFACE_LONGITUDE[,][results3$SURFACE_LATITUDE[,]>0],results3$SURFACE_LATITUDE[,][results3$SURFACE_LATITUDE[,]>0]), proj4string = CRS(origproj))
wells_points <- spTransform(wells_points, CRS(googleproj))
local.proj <- paste("+proj=aea +lat_1=", bbox(wells_points)[2,1], " +lat_2=", bbox(wells_points)[2,2], " +lat_0=", mean(bbox(wells_points)[2,]), " +lon_0=", mean(bbox(wells_points)[1,]), " +x_0=0 +y_0=0 +datum=WGS84 +units=m",sep="")
wells_points <- spTransform(wells_points, CRS(local.proj))
x <- coordinates(wells_points)[,1]
y <- coordinates(wells_points)[,2]
well_results <- SpatialPointsDataFrame(wells_points, data.frame(latitude=coordinates(wells_points)[,2], longitude=coordinates(wells_points)[,1], x=x, y=y, EUR_exp=results3$EUR_exp[,][results3$SURFACE_LATITUDE[,]>0], EUR_harm=results3$EUR_harm[,][results3$SURFACE_LATITUDE[,]>0], Rem_exp=results3$Rem_exp[,][results3$SURFACE_LATITUDE[,]>0], Rem_harm=results3$Rem_harm[,][results3$SURFACE_LATITUDE[,]>0]))

wells_points_mon <- SpatialPoints(cbind(results_mon$SURFACE_LONGITUDE[,][results_mon$SURFACE_LATITUDE[,]>0],results_mon$SURFACE_LATITUDE[,][results_mon$SURFACE_LATITUDE[,]>0]), proj4string = CRS(origproj))
wells_points_mon <- spTransform(wells_points_mon, CRS(googleproj))
local.proj <- paste("+proj=aea +lat_1=", bbox(wells_points_mon)[2,1], " +lat_2=", bbox(wells_points_mon)[2,2], " +lat_0=", mean(bbox(wells_points_mon)[2,]), " +lon_0=", mean(bbox(wells_points_mon)[1,]), " +x_0=0 +y_0=0 +datum=WGS84 +units=m",sep="")
wells_points_mon <- spTransform(wells_points_mon, CRS(local.proj))
x <- coordinates(wells_points_mon)[,1]
y <- coordinates(wells_points_mon)[,2]
well_results_mon <- SpatialPointsDataFrame(wells_points_mon, data.frame(latitude=coordinates(wells_points_mon)[,2], longitude=coordinates(wells_points_mon)[,1], x=x, y=y, EUR_exp=results_mon$EUR_exp[,][results_mon$SURFACE_LATITUDE[,]>0], EUR_harm=results_mon$EUR_harm[,][results_mon$SURFACE_LATITUDE[,]>0], Rem_exp=results_mon$Rem_exp[,][results_mon$SURFACE_LATITUDE[,]>0], Rem_harm=results_mon$Rem_harm[,][results_mon$SURFACE_LATITUDE[,]>0]))












hpt <- spsample(well_results, type="hexagonal", n=50000)  #hexpoints...
hpg <- HexPoints2SpatialPolygons(hpt)   #spatialpolygons
EUR.hexid <- over(well_results, hpg)
EUR.split <- split(well_results@data, EUR.hexid)
names(EUR.split) <- sapply(hpg@polygons, function(x) x@ID)[as.numeric(names(EUR.split))]
EUR_exp.sum <- sapply(EUR.split, function(x) log(sum(na.omit(x$EUR_exp))))
EUR_exp.sum <- data.frame(EUR_exp.sum)
EUR_exp.spdf <- SpatialPolygonsDataFrame(hpg[rownames(EUR_exp.sum)], EUR_exp.sum)
Rem_exp.sum <- sapply(EUR.split, function(x) log(sum(na.omit(x$Rem_exp))))
Rem_exp.sum <- data.frame(Rem_exp.sum)
Rem_exp.spdf <- SpatialPolygonsDataFrame(hpg[rownames(Rem_exp.sum)], Rem_exp.sum)

#plotKML(EUR_exp.spdf)
plotGoogleMaps(EUR_exp.spdf,filename='EUR_exp.htm',openMap=FALSE)
plotGoogleMaps(Rem_exp.spdf,filename='Rem_exp.htm',openMap=FALSE)







box <-bbox(EUR_exp.spdf)
EUR.breaks <- c(1,1000*round(classIntervals(EUR_exp.sum[EUR_exp.sum>1], n=num_intervals, style="quantile")$brks/1000)[-1])
cl <- map("worldHires", xlim = c(box[1], box[3]), ylim = c(box[2], box[4]), plot = FALSE)
clp1 <- map2SpatialLines(cl)
cl <- map("state", xlim = c(box[1], box[3]), ylim = c(box[2], box[4]), interior = T, boundary = F, plot = FALSE)
clp2 <- map2SpatialLines(cl)

spplot(
EUR_exp.spdf, 
"EUR_exp.sum", 
col = "transparent", 
at = EUR.breaks, 
lwd = 0.1, 
col.regions = c("darkslategray1", rev(heat_hcl(5))), 
colorkey = list(space = "bottom", labels = list(at = EUR.breaks), cex = 0.9), 
xlim = c(box[1] * 0.97, box[3]), 
ylim = c(box[2], box[4] * 0.8), 
sub = list(label = "EUR per Hexagonal Block", cex = 1), 
panel = function(x, y, ...) {
        panel.polygonsplot(x, y, ...)
        sp.lines(clp1, col = "black", lwd = 0.2)
        sp.lines(clp2, col = "black", lwd = 0.3)
    }
)







hpt_mon <- spsample(well_results_mon, type="hexagonal", nsig=2, cellsize=2*2*(12^-0.25)*1609.344)  #2560 acre hexpoints...
hpg_mon <- HexPoints2SpatialPolygons(hpt_mon)   #spatialpolygons
EUR_mon.hexid <- over(well_results_mon, hpg_mon)
EUR_mon.split <- split(well_results_mon@data, EUR_mon.hexid)
names(EUR_mon.split) <- sapply(hpg_mon@polygons, function(x) x@ID)[as.numeric(names(EUR_mon.split))]
EUR_exp_mon.sum <- sapply(EUR_mon.split, function(x) log(sum(na.omit(x$EUR_exp))))
EUR_exp_mon.sum <- data.frame(EUR_exp_mon.sum)
EUR_exp_mon.spdf <- SpatialPolygonsDataFrame(hpg_mon[rownames(EUR_exp_mon.sum)], EUR_exp_mon.sum)
Rem_exp_mon.sum <- sapply(EUR_mon.split, function(x) log(sum(na.omit(x$Rem_exp))))
Rem_exp_mon.sum <- data.frame(Rem_exp_mon.sum)
Rem_exp_mon.spdf <- SpatialPolygonsDataFrame(hpg_mon[rownames(Rem_exp_mon.sum)], Rem_exp_mon.sum)
EUR_harm_mon.sum <- sapply(EUR_mon.split, function(x) log(sum(na.omit(x$EUR_harm))))
EUR_harm_mon.sum <- data.frame(EUR_harm_mon.sum)
EUR_harm_mon.spdf <- SpatialPolygonsDataFrame(hpg_mon[rownames(EUR_harm_mon.sum)], EUR_harm_mon.sum)
Rem_harm_mon.sum <- sapply(EUR_mon.split, function(x) log(sum(na.omit(x$Rem_harm))))
Rem_harm_mon.sum <- data.frame(Rem_harm_mon.sum)
Rem_harm_mon.spdf <- SpatialPolygonsDataFrame(hpg_mon[rownames(Rem_harm_mon.sum)], Rem_harm_mon.sum)
EUR_error_mon.sum <- sapply(EUR_mon.split, function(x) mean((na.omit(x$EUR_harm-x$EUR_exp))/(na.omit(x$EUR_harm+x$EUR_exp))))
EUR_error_mon.sum <- data.frame(EUR_error_mon.sum)
EUR_error_mon.spdf <- SpatialPolygonsDataFrame(hpg_mon[rownames(EUR_error_mon.sum)], EUR_error_mon.sum)
EUR_var_mon.sum <- sapply(EUR_mon.split, function(x) (sd(na.omit(x$EUR_exp))/mean(na.omit(x$EUR_exp))+sd(na.omit(x$EUR_harm))/mean(na.omit(x$EUR_harm)))/2.0)
EUR_var_mon.sum <- data.frame(EUR_var_mon.sum)
EUR_var_mon.spdf <- SpatialPolygonsDataFrame(hpg_mon[rownames(EUR_var_mon.sum)], EUR_var_mon.sum)
#plotKML(EUR_exp_mon.spdf)
plotGoogleMaps(EUR_exp_mon.spdf,filename='EUR_exp_mon.htm',openMap=FALSE)
plotGoogleMaps(Rem_exp_mon.spdf,filename='Rem_exp_mon.htm',openMap=FALSE)
plotGoogleMaps(EUR_harm_mon.spdf,filename='EUR_harm_mon.htm',openMap=FALSE)
plotGoogleMaps(Rem_harm_mon.spdf,filename='Rem_harm_mon.htm',openMap=FALSE)
plotGoogleMaps(EUR_error_mon.spdf,filename='EUR_error_mon.htm',openMap=FALSE)
plotGoogleMaps(EUR_var_mon.spdf,filename='EUR_var_mon.htm',openMap=FALSE)

hpt_mon <- spsample(well_results_mon, type="hexagonal", nsig=2, cellsize=2*(12^-0.25)*1609.344/4.0)  #40 acre hexpoints...
hpg_mon <- HexPoints2SpatialPolygons(hpt_mon)   #spatialpolygons
EUR_mon.hexid <- over(well_results_mon, hpg_mon)
EUR_mon.split <- split(well_results_mon@data, EUR_mon.hexid)
names(EUR_mon.split) <- sapply(hpg_mon@polygons, function(x) x@ID)[as.numeric(names(EUR_mon.split))]
EUR_exp_mon.sum <- sapply(EUR_mon.split, function(x) log(sum(na.omit(x$EUR_exp))))
EUR_exp_mon.sum <- data.frame(EUR_exp_mon.sum)
e <- SpatialPointsDataFrame(hpt_mon[as.numeric(gsub("^.*?ID","",rownames(EUR_exp_mon.sum)))], EUR_exp_mon.sum)
e <- e[-which(is.infinite(e@data$EUR_exp_mon.sum)),]
png("EUR_bubble_exp_mon.png", width = 5000, height = 4000,res=600) #single plot
bubble(e, "EUR_exp_mon.sum", col=c("#00ff0088", "#00ff0088"), main = "Exp EUR per Section (MMcf)")

dev.off()
## create a grid onto which we will interpolate:
## first get the range in data
x.range <- as.integer(range(e@coords[,1]))
y.range <- as.integer(range(e@coords[,2]))
## now expand to a grid with 500 meter spacing:
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=500), y=seq(from=y.range[1], to=y.range[2], by=1*1609.344) )
## convert to SpatialPixel class
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE
proj4string(grd) <- proj4string(e)
gc()
g <- gstat(id="EUR_exp_mon.sum", formula=EUR_exp_mon.sum ~ 1, data=e)
#plot(variogram(g, map=TRUE, cutoff=4000, width=200), threshold=10)
v <- variogram(g, alpha=c(0,45,90,135))
v.fit <- fit.variogram(v, model=vgm(model='Lin', nugget=1, anis=c(0, 0.5)))
#v.fit <- fit.variogram(v, model=vgm(model='Exp', nugget=5000, range=25000, anis=c(90, 0.25)))
#v.fit <- fit.variogram(v, model=vgm(model='Gau', nugget=1000, range=25000, anis=c(0, 0.5)))
png("EUR_exp_Variogram_mon.png", width = 5000, height = 4000,res=600) #single plot
plot(v, model=v.fit, as.table=TRUE)
dev.off()
g <- gstat(g, id="EUR_exp_mon.sum", model=v.fit )
gc()
p <- predict(g, model=v.fit, newdata=grd)
## visualize it:
pts <- list("sp.points", e, pch = 4, col = "black", cex=0.5)
png("EUR_exp_OK_Pred_mon.png", width = 5000, height = 4000,res=600) #single plot
spplot(p, zcol="EUR_exp_mon.sum.pred", col.regions=terrain.colors(20), cuts=19, sp.layout=list(pts), contour=TRUE, labels=FALSE, pretty=TRUE, col='brown', main='Ordinary Kriging Prediction')
dev.off()
## plot the kriging variance as well
png("EUR_exp_OK_Var_mon.png", width = 5000, height = 4000,res=600) #single plot
spplot(p, zcol='EUR_exp_mon.sum.var', col.regions=heat.colors(20), cuts=19, sp.layout=list(pts), contour=TRUE, labels=FALSE, pretty=TRUE, col='brown', main='Ordinary Kriging Variance')
#spplot(p, zcol='EUR_exp_mon.sum.var', col.regions=terrain.colors(20), cuts=19, sp.layout=list(pts), contour=TRUE, labels=FALSE, pretty=TRUE, col='brown', main='Ordinary Kriging Variance')
dev.off()




















#require(GSIF)
#LSTDm <- reproject(spdf, program="FWTools")
#LSTDm <- as(LSTDm, "SpatialPixelsDataFrame")
#LSTDm <- tile(spdf, block.x=5e5)
#plotKML(LSTDm.l, z.lim = zlims, colour_scale=SAGA_pal[[1]])


#LSTDm <- SpatialPointsDataFrame(wells_points[!is.na(well_results$EUR_exp)], data.frame(latitude=coordinates(wells_points)[,2][!is.na(well_results$EUR_exp)], longitude=coordinates(wells_points)[,1][!is.na(well_results$EUR_exp)], x=x[!is.na(well_results$EUR_exp)], y=y[!is.na(well_results$EUR_exp)], EUR_exp=well_results$EUR_exp[!is.na(well_results$EUR_exp)]))

#writeOGR(well_results["EUR_exp"], "EUR_exp.kml", layer="EUR_exp", driver="KML") 
#writeOGR(spdf, "EUR_exp_2.kml", layer="EUR_exp", driver="KML") 
#require("plotGoogleMaps")
#m<-plotGoogleMaps(LSTDm,filename='myMap1.htm',openMap=FALSE)

#LSTDm <- reproject(well_results["EUR_exp"], program="FWTools")
#LSTDm <- tile(well_results["EUR_exp"], block.x=5e5)
#plotKML(LSTDm.l, z.lim = zlims, colour_scale=SAGA_pal[[1]])

