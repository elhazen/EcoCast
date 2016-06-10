###################################### SPATIAL PREDICTIONS

library(ncdf4) 
library(spdep)
require(maps)      
require(mapdata) 
library(raster)
library(rasterVis)
library(gbm)
library(colorRamps)
library(gtools)
library(scales)
#library(rgdal)

source('~/Dropbox/Shared Folders/DOM_dl/LBST/xtract/Xtracto_Local.R')

### get lat and lon of each cell in base output raster - here, uses CCS etopo extent 
### xy just needs to be an 'expand.grid' type list of each combo of lat/lon (for each cell)

#EPSG <- make_EPSG()
oldproj<-CRS("+proj=longlat +datum=WGS84")

r <-raster(extent(-135,-115,30,50),resolution=c(0.25,0.25)); projection(r) <- oldproj
xy <- as.data.frame(xyFromCell(r, 1:ncell(r[[1]]),spatial=TRUE))
colnames(xy) <- c('lon','lat')

pl <- rbind(c(-115,30), c(-125,30), c(-130,38), c(-130,45), c(-115,45), c(-115,30))
pl <- SpatialPolygons(list(Polygons(list(Polygon(pl)), 1)))

## extract z, zsd per lon/lat  ## NB. THESE WILL STAY THE SAME PER DAY
source('~/Dropbox/Shared Folders/DOM_dl/LBST/getZ_etopo1_LBST.r', chdir = TRUE)
scb_pr <- data.frame(lon=xy$lon, lat=xy$lat, z=xy$z, zsd=xy$zsd)
write.csv(scb_pr,file="lbst_pr.csv")
#scb_pr$z <- extract(etopo,xy,method='simple',na.rm=F,cellnumbers=F)
#scb_pr$zsd <- extract(etopo_sd,xy,method='simple',na.rm=F,cellnumbers=F)

## RUN XTRACTO using scb_pr (ultimately, xy) as input data
## !! MAY NEED TO STANDARDISE RESOLUTION OF INPUT EOVs (Raster::calc) - HERE, ONLY SST 

## list NetCDF files
ncpath <- '/Volumes/LaCie4TB/Data/NCDF/'
list.ncs <- list.files(ncpath, full.names=T)
# filenames<-grep("PH2",list.ncs)  ## for whole date range
filenames<-grep("jpl",list.ncs)  ## for 2012 ONWARDS
sstfiles<-list.ncs[filenames]
sstvarname<-"analysed_sst"; mnradius<-0; alt<-NA
# varname<-"sea_surface_temperature"; mnradius<-0; alt<-NA

#CHL
filenames<-grep("erdM",list.ncs)  ## for 2012 ONWARDS
chlfiles<-list.ncs[filenames]
chlvarname<-"chlorophyll"; mnradius<-0; chalt<-1

#SSH madt_u
filenames<-grep("madt_u",list.ncs)  ## for 2012 ONWARDS
sshufiles<-list.ncs[filenames]
sshuvarname<-"u"; mnradius<-0; alt<-NA
filenames<-grep("madt_u",list.ncs)  ## for 2012 ONWARDS
sshvfiles<-list.ncs[filenames]
sshvvarname<-"v"; mnradius<-0; alt<-NA
filenames<-grep("madt_h",list.ncs)  ## for 2012 ONWARDS
sshafiles<-list.ncs[filenames]
sshavarname<-"adt"; mnradius<-0; alt<-NA

#windy_new_mean 
filenames<-grep("erdQAwind8day",list.ncs)  ## for 2012 ONWARDS
windfiles<-list.ncs[filenames]
windvarname<-"y_wind"; mnradius<-0; walt<-10

 test <- nc_open(sstfiles[12])
 print(test)
 nc_close(test)

#r <-raster(extent(-121,-116,30,35),resolution=c(0.05,0.05)); projection(r) <- oldproj
#scb$month <- as.character(scb$month)

getPredictor <- function(xdata=scb_pr, x="01/01/2015",varname=sstvarname,filesin=sstfiles,alt=alt){
    date1 <- as.Date(x)
    print(paste('Starting',date1))
#    scb_pr <- read.csv('./scb_pr.csv')   ## model predictions
    xdata$dt <- date1
    ## LUNAR ILLUMINATION
    #scb_pr$lunillum <- lunar.illumination(scb_pr$dt)
    ### GHRSST EXTRACT  ## could do with rasterize NetCDF (rotate, transverse) if this too slow
    obsdata <- xdata
    obsdata$X <- seq(1,nrow(obsdata))
    #obsdata$lon <- obsdata$lon-360
    obsdata$dt <- as.POSIXct(paste(obsdata$dt,'12:00:00'),tz='UTC')
    predfilesIn <- getDateRange(inpts=obsdata,ncIn=filesin) 
    predmean <- rbindlist(lapply(predfilesIn, FUN=getvar,varname=varname,ptsfile=obsdata,pt.radius=mnradius,alt=alt),fill=TRUE)
    datavalue<-rep(NA, length(obsdata[,1]))
    #obsdata$sst<-rep(NA, length(obsdata[,1]))
    # obsdata$HiSST[sstmean$X]<-sstmean$SST_mean
    varname2<-paste(varname,"mean",sep='_')
    datavalue<-as.vector(as.data.frame(predmean)[varname2])
    
    ### Calculate SD
    r2<-rasterFromXYZ(predmean)
    r3<-focal(r2[[5]],w=matrix(1/225,nrow=15,ncol=15),fun=sd,na.rm=TRUE)
    r3[is.na(r3)]<-(-999)
    xy3<-rasterToPoints(r3)
    names(xy3)<-c("lon","lat","predmean")
    xy3[xy3[,3]==-999,3]<-NA
    predmean$sd <- xy3[,3]
    datavalue$sd[predmean$X]<-predmean$sd
    
    return(datavalue)    
}

#test<-getPredictor(scb_pr,date1,sstvarname,sstfiles)
#test<-getPredictor(scb_pr,date1,chlvarname,chlfiles,chalt)
#test<-getPredictor(scb_pr,date1,sshuvarname,sshufiles)
#test<-getPredictor(scb_pr,date1,sshuvarname,sshufiles)


##!!! FUNCTION NOT YET GENERALISED
predDaily <- function(x){
    date1 <- as.Date(x)
    print(paste('Starting',date1))
#    scb_pr <- read.csv('./scb_pr.csv')   ## model predictions
    scb_pr$dt <- date1
    ## LUNAR ILLUMINATION
    #scb_pr$lunillum <- lunar.illumination(scb_pr$dt)
    ### GHRSST EXTRACT  ## could do with rasterize NetCDF (rotate, transverse) if this too slow
    obsdata <- scb_pr
    obsdata$X <- seq(1,nrow(obsdata))
    #obsdata$lon <- obsdata$lon-360
    obsdata$dt <- as.POSIXct(paste(obsdata$dt,'12:00:00'),tz='UTC')
    sstfilesIn <- getDateRange(inpts=obsdata,ncIn=sstfiles) 
    sstmean <- rbindlist(lapply(sstfilesIn, FUN=getvar,varname=varname,ptsfile=obsdata,pt.radius=mnradius),fill=TRUE)
    obsdata$sst<-rep(NA, length(obsdata[,1]))
    # obsdata$HiSST[sstmean$X]<-sstmean$SST_mean
    obsdata$sst[sstmean$X]<-sstmean$analysed_sst_mean
    
    ### SST SD
    r2<-rasterFromXYZ(sstmean)
    r3<-focal(r2[[5]],w=matrix(1/225,nrow=15,ncol=15),fun=sd,na.rm=TRUE)
    r3[is.na(r3)]<-(-999)
    xy3<-rasterToPoints(r3)
    names(xy3)<-c("lon","lat","sst")
    xy3[xy3[,3]==-999,3]<-NA
    sstmean$analysed_sst_sd <- xy3[,3]
    obsdata$sst_sd[sstmean$X]<-sstmean$analysed_sst_sd
    
    ### logChl
    chlfilesIn <- getDateRange(inpts=obsdata,ncIn=chlfiles) 
    sstmean <- rbindlist(lapply(chlfilesIn, FUN=getvar,varname=varname,ptsfile=obsdata,pt.radius=mnradius),fill=TRUE)

    ### log_eke
    sstfilesIn <- getDateRange(inpts=obsdata,ncIn=sstfiles) 
    sstmean <- rbindlist(lapply(sstfilesIn, FUN=getvar,varname=varname,ptsfile=obsdata,pt.radius=mnradius),fill=TRUE)

    ### windy_new_mean
    sstfilesIn <- getDateRange(inpts=obsdata,ncIn=sstfiles) 
    sstmean <- rbindlist(lapply(sstfilesIn, FUN=getvar,varname=varname,ptsfile=obsdata,pt.radius=mnradius),fill=TRUE)



    scb_pr <- obsdata; rm(sstmean,sstfilesIn,obsdata)

    ### PREDICT (without RN)  ## FOR BRT
    mod.pred <- predict.gbm(scb.full3,scb_pr,n.trees=scb.full3$gbm.call$best.trees,type='response')
    mod.pred <- as.vector(mod.pred)
    fitdata <- cbind(scb_pr,as.data.frame(mod.pred))
    write.csv(fitdata,paste('./DailyPreds/SWOR_HiRes_',as.character(date1),'.csv',sep=''),row.names=F, quote=F)
    coordinates(fitdata) = c('lon','lat'); proj4string(fitdata) <- oldproj
    print(head(scb_pr)) 
    ### PLOT
    ## No PTS   
    png(paste('~/Dropbox/R/DOM/SWOR/SWOR_v2/BIGHT/DailyPreds/noPts/SWOR_HiRes_',as.character(date1),'.png', sep=''),width=960,height=960,units='px',bg='white')
    par(mar=c(3,3,1,1),las=1, mgp=c(0.3,1,0))
    plot(d, col=matlab.like2(255),xlim=c(-121,-116),ylim=c(30,35),zlim=c(0,1), cex.axis=2, font=2) 
    map('world',c('USA','Mexico','Canada','Hawaii'), add=TRUE, fill=TRUE, col='azure4', interior=TRUE, resolution=0)
    text(-117,34.8,as.character(date1),font=2,cex=2,adj=c(1,1))
    text(-117,34.4,paste(as.integer((length(which(is.na(scb_pr$HiSST)))-length(which(is.na(scb_pr$z))))/length(scb_pr$z)*100), '% SST missing',sep=''),font=2,cex=2,adj=c(1,1))
    box()
    dev.off()
}

finalBRT<-bstrap.brt.full[[which.max(bstrap.brt.cv)]]

predBRT <- function(x){
    date1 <- as.Date(x)
    print(paste('Starting',date1))
#    scb_pr <- read.csv('./scb_pr.csv')   ## model predictions
    scb_pr$dt <- as.POSIXct(paste(date1,'12:00:00'),tz='UTC')
    scb_pr$X <- seq(1,nrow(scb_pr))
    #obsdata$lon <- obsdata$lon-360
    ## LUNAR ILLUMINATION
    #scb_pr$lunillum <- lunar.illumination(scb_pr$dt)
    ### GHRSST EXTRACT  ## could do with rasterize NetCDF (rotate, transverse) if this too slow
    sst<-getPredictor(scb_pr,date1,sstvarname,sstfiles)
  
    ### logChl
    chl<-getPredictor(scb_pr,date1,chlvarname,chlfiles,1)
    logChl<-log(chl[,1]+0.0001)

    ### log_eke
    testu<-getPredictor(scb_pr,date1,sshuvarname,sshufiles)
    testv<-getPredictor(scb_pr,date1,sshvvarname,sshvfiles)
    eke<-1/2*(testu$u_mean^2+testv$v_mean^2)
    log_eke<-log(eke+0.0001)

    ### ssh_u
    testh<-getPredictor(scb_pr,date1,sshavarname,sshafiles)
    

    ### windy_new_mean
    windy<-getPredictor(scb_pr,date1,windvarname,windfiles,1)
    windy_new_mean<-windy$y_wind_mean

    ### RN
    RN<-sample(c(1:100),size=dim(sst)[1], replace=TRUE)

    #scb_pr <- obsdata; rm(sstmean,sstfilesIn,obsdata)
    scb_pr <- cbind(scb_pr,sst[,1:2],logChl,log_eke,windy_new_mean,testh,RN)
    names(scb_pr)<-c("lon","lat","z","zsd","dt","X","sst","sst_sd","logChl","log_eke","windy_new_mean","AVISOh_new_mean","AVISOh_new_sd","RN")

    ### PREDICT (without RN)  ## FOR BRT
    mod.pred <- predict.gbm(finalBRT,scb_pr,n.trees=finalBRT$gbm.call$best.trees,type='response')
    mod.pred <- as.vector(mod.pred)
    fitdata <- cbind(scb_pr,as.data.frame(mod.pred))
    write.csv(fitdata,paste('./LBST/PredictV1/',as.character(date1),'.csv',sep=''),row.names=F, quote=F)
    coordinates(fitdata) = c('lon','lat'); proj4string(fitdata) <- oldproj
    print(head(scb_pr)) 

    ## rasterize the shit out of it
    r <-raster(nrows=length(unique(fitdata$lat)),ncols=length(unique(fitdata$lon+1)), extent(-135, -115, 30,50))
    d <- rasterize(fitdata, r, fitdata$mod.pred, fun=mean)
    ccM <- mask(d,pl)

    ### PLOT
    ## No PTS   
    png(paste('./LBST/PredictV1/','LBST_025_',as.character(date1),'.png', sep=''),width=960,height=960,units='px',bg='white')
    par(mar=c(3,3,1,1),las=1, mgp=c(0.3,1,0))
    plot(d, col=matlab.like2(255),xlim=c(-150,-110),ylim=c(10,60),zlim=c(0,1), cex.axis=2, font=2) 
    map('world',c('USA','Mexico','Canada','Hawaii'), add=TRUE, fill=TRUE, col='azure4', interior=TRUE, resolution=0)
    text(-115,47.8,as.character(date1),font=2,cex=2,adj=c(1,1))
    text(-115,47.4,paste(as.integer((length(which(is.na(scb_pr$sst)))-length(which(is.na(scb_pr$z))))/length(scb_pr$z)*100), '% SST missing',sep=''),font=2,cex=2,adj=c(1,1))
    box()
    dev.off()

    #return(cbind(scb_pr,as.data.frame(mod.pred)))
}


## RUN FUNC OVER LIST OF DATES
dates <- seq(from=as.Date('2012-08-01'),to=as.Date('2012-10-31'),by='8 day')
# dates2 <- seq(from=as.Date('2013-09-01'),to=as.Date('2014-01-31'),by='1 day')
# dates <- paste(dates,dates2)

## TEST
#predDaily(dates[1])
#x <- dates[1]

# dates <- seq(from=as.Date('2013-11-01'),to=as.Date('2014-01-31'),by='1 day')    
lapply(dates, FUN=predBRT)  ### <----------------------
