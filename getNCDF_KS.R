setwd("/Volumes/NCDF_drive/DOM/NCDF")
setwd("/Volumes/LaCie4TB/Data/NCDF/Bight")

###LASDEV
setwd("~/R/prediction_layers/NCDF")

###### NEED TO BUILD IN WAITS

focalsd <- function(ncvals,xmin,xmax,ymin,ymax,xres=7,yres=7){
  if (!is.installed("raster")){
    install.packages("raster")
  }
#  ncvals<-get.var.ncdf(ncdffile)
#  v1<-ncdffile$var[[1]]
#  varsize <- v1$varsize
#  ndims <- v1$ndims
#  nt <- varsize[ndims]

#  nclats<-v1$dim[[1]]$vals
#  nclons<-v1$dim[[2]]$vals
  ncraster<-raster(ncvals,xmn=xmin,xmx=xmax,ymn=ymin,ymx=ymax)
  r<-ncraster
  #ncells<-1/res(sstraster)[1]
  #ncextent<-extent(min(nclats),max(nclats),min(nclons),max(nclons))
  
  #r<-setExtent(r,ncextent)
  #ncells<-1/res(r)
  
  rsd = focal(r, w=matrix(1,nrow=xres,ncol=yres), fun=sd,na.rm=TRUE)
  return(rsd)
}

waitfor <- function(x){
    p1 <- proc.time()
    Sys.sleep(x)
    print(proc.time() - p1) # The cpu usage should be negligible
}


# getdateseq<-function(dates,dtype,var,latlim,lonlim){
# 	i<-1
# 	while (i < length(dates)){
# 		startdate<-dates[i]
# 		enddate<-dates[i+1]
# #		enddate<-seq.Date(as.Date(startdate),by="month",length.out=2)[2]
# 		filenm<-paste(dtype,"_",startdate,".nc",sep="")
# 		url<-paste("http://coastwatch.pfeg.noaa.gov/erddap/griddap/",dtype,".nc?",var,"[(",startdate,"):1:(",enddate,")][(0.0):1:(0.0)][(22.0):1:(51.0)][(215.0):1:(255.0)]",sep="")
# 		print(startdate)
# 		f = CFILE(filenm,mode="wb")
# 		curlPerform(url=url,writedata=f@ref) 
# 		close(f)
# 		if (is.na(file.info(filenm)$size)) {
# 			i<-i-1
# 		}
# 		else if (file.info(filenm)$size < 2000){
# 			i<-i-1
# 		}
# 	}

# # 	file.info("jplL4AvhrrOIv1fv2_1992-08-01.nc")$size
# }

#Install and load libraries
is.installed <- function(x){
    is.element(x, installed.packages()[,1])
  } 

if (!is.installed("ncdf")){
    install.packages("ncdf")
  }
if (!is.installed("RCurl")){
    install.packages("RCurl")
  }

if (!is.installed("gmt")){
    install.packages("gmt")
  }
if (!is.installed("SDMTools")){
    install.packages("SDMTools")
  }


if (!is.installed("raster")){
    install.packages("raster")
  }
  
library(gmt)
library(SDMTools)
library(ncdf4)
library(RCurl)
library(raster)


#ERDDAP PFEG download
#yrs<-seq(1998,2009)
#mos<-seq(1,12)
#mos<-formatC(mos, width = 2, flag = '0')

### ETOPO - http://coastwatch.pfeg.noaa.gov/erddap/griddap/etopo180.nc?altitude[(30):1:(35)][(-121):1:(-116)]

################################################ HERE

#Pathfinder 4k - http://coastwatch.pfeg.noaa.gov/erddap/griddap/nodcPH2sstd1day.html
#http://coastwatch.pfeg.noaa.gov/erddap/griddap/nodcPH2sstd1day.nc?sea_surface_temperature[(1990-01-01T12:00:00Z):1:(2012-12-31T12:00:00Z)][(30):1:(35)][(-121):1:(-116)]
dates<-seq(as.Date("1991/10/01"), as.Date("2010/7/31"), by = "month",format="%Y/%mm/%dd")
# url<-"http://coastwatch.pfeg.noaa.gov/erddap/griddap/nodcPH2sstd1day.nc?sea_surface_temperature[(1990-01-01T12:00:00Z):1:(2012-12-31T12:00:00Z)][(10):1:(60)][(-150):1:(-100)]"
# url <- 'https://coastwatch.pfeg.noaa.gov/erddap/griddap/nodcPH2sstd1day.nc?sea_surface_temperature[(1990-01-01T12:00:00Z):1:(1990-03-01T12:00:00Z)][(60):1:(10)][(-150):1:(-110)]'
# filenm<-paste("nodcPH2sstd1day_","1990_1_01_all",".nc",sep="")
#dates<-c("1990/1/01","2012/1/31")
### WHILE LOOP
 i<-1
waitsecs<-2

while (i < length(dates)){
	startdate<-dates[i]
	enddate<-dates[i+1]
	filenm<-paste("nodcPH2sstd1day_",startdate,".nc",sep="")
	url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nodcPH2sstd1day.nc?sea_surface_temperature[(",startdate,"):1:(",enddate,")][(60):1:(10)][(-150):1:(-110)]",sep="")
	print(startdate)
	f = CFILE(filenm,mode="wb")
	curlPerform(url=url,writedata=f@ref) 
 	close(f)
	i<-i+1
	if (is.na(file.info(filenm)$size)) {
		i<-i-1
		waitfor(waitsecs)
		waitsecs<-waitsecs+2
	}
	else if (file.info(filenm)$size < 2000){
		i<-i-1
		waitfor(waitsecs)
		waitsecs<-waitsecs+2
	}
	else waitsecs<-2
	if (waitsecs > 90) waitsecs <- 30
}

#GHRSST - http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplG1SST.html
url<-"http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplG1SST.nc?SST[(2012-01-01):1:(2016-01-13T00:00:00Z)][(30):1:(35)][(-121):1:(-116)]"
filenm<-paste("jplG1SST_","1990_1_01_all",".nc",sep="")
### RUN MISSING DATES
#dates<-c("1986-05-01","2001-06-01")
dates<-seq(as.Date("2010/07/01"), as.Date("2015/12/31"), by = "week",format="%Y/%mm/%dd")
i<-1
while (i < length(dates)){
	startdate<-dates[i]
	enddate<-dates[i+1]
	#enddate<-seq.Date(as.Date(startdate),by="week",length.out=2)[2]
	filenm<-paste("jplG1SST_",startdate,".nc",sep="")
	url<-paste("http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplG1SST.nc?SST[(",startdate,"):1:(",enddate,")][(10):1:(60)][(-150):1:(-110)]",sep="")
				####http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplL4AvhrrOIv1fv2.nc?analysed_sst[(1981-09-01):1:(2015-04-05T00:00:00Z)][(10):1:(60)][(-150):1:(-100)]
	print(startdate)
	f = CFILE(filenm,mode="wb")
	curlPerform(url=url,writedata=f@ref) 
	close(f)
		i<-i+1
	if (is.na(file.info(filenm)$size)) {
		i<-i-1
	}
	else if (file.info(filenm)$size < 2000){
		i<-i-1
	}
}

url<-"http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplG1SST.nc?SST[(2012-01-01):1:(2016-01-13T00:00:00Z)][(30):1:(35)][(-121):1:(-116)]"
filenm<-paste("jplG1SST_","1990_1_01_all",".nc",sep="")
### RUN MISSING DATES
#dates<-c("1986-05-01","2001-06-01")
dates<-seq(as.Date("2010/07/01"), as.Date("2015/12/31"), by = "week",format="%Y/%mm/%dd")
i<-1
while (i < length(dates)){
  startdate<-dates[i]
  enddate<-dates[i+1]
  #enddate<-seq.Date(as.Date(startdate),by="week",length.out=2)[2]
  filenm<-paste("jplG1SST_",startdate,".nc",sep="")
  url<-paste("http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplG1SST.nc?SST[(",startdate,"):1:(",enddate,")][(10):1:(60)][(-150):1:(-110)]",sep="")
        ####http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplL4AvhrrOIv1fv2.nc?analysed_sst[(1981-09-01):1:(2015-04-05T00:00:00Z)][(10):1:(60)][(-150):1:(-100)]
  print(startdate)
  f = CFILE(filenm,mode="wb")
  curlPerform(url=url,writedata=f@ref) 
  close(f)
    i<-i+1
  if (is.na(file.info(filenm)$size)) {
    i<-i-1
  }
  else if (file.info(filenm)$size < 2000){
    i<-i-1
  }
}

#http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMBchla8day.nc?chlorophyll[(2014-05-01):1:(2016-06-05T00:00:00Z)][(0.0):1:(0.0)][(10):1:(60)][(210):1:(250)]
filenm<-paste("erdMBchla8day_","2014_1_01_all",".nc",sep="")
### RUN MISSING DATES
#dates<-c("1986-05-01","2001-06-01")
dates<-seq(as.Date("2014/05/01"), as.Date("2015/12/31"), by = "week",format="%Y/%mm/%dd")
i<-1
while (i < length(dates)){
  startdate<-dates[i]
  enddate<-dates[i+1]
  #enddate<-seq.Date(as.Date(startdate),by="week",length.out=2)[2]
  filenm<-paste("erdMBchla8day_",startdate,".nc",sep="")
  url<-paste("http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMBchla8day.nc?chlorophyll[(",startdate,"):1:(",enddate,")][(0.0):1:(0.0)][(10):1:(60)][(210):1:(250)]",sep="")
        ####http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplL4AvhrrOIv1fv2.nc?analysed_sst[(1981-09-01):1:(2015-04-05T00:00:00Z)][(10):1:(60)][(-150):1:(-100)]
  print(startdate)
  f = CFILE(filenm,mode="wb")
  curlPerform(url=url,writedata=f@ref) 
  close(f)
    i<-i+1
  if (is.na(file.info(filenm)$size)) {
    i<-i-1
  }
  else if (file.info(filenm)$size < 2000){
    i<-i-1
  }
}

#http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQAwind8day.nc?y_wind[(2016-06-05T00:00:00Z):1:(2016-06-05T00:00:00Z)][(10.0):1:(10.0)][(10):1:(60)][(210):1:(250)]
filenm<-paste("erdQAwind8day_","2014_1_01_all",".nc",sep="")
dates<-seq(as.Date("2009/11/01"), as.Date("2016/05/31"), by = "month",format="%Y/%mm/%dd")
i<-1
while (i < length(dates)){
  startdate<-dates[i]
  enddate<-dates[i+1]
  #enddate<-seq.Date(as.Date(startdate),by="week",length.out=2)[2]
  filenm<-paste("erdQAwind8day_",startdate,".nc",sep="")
  url<-paste("http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQAwind8day.nc?y_wind[(",startdate,"):1:(",enddate,")][(10.0):1:(10.0)][(10):1:(60)][(210):1:(250)]",sep="")
        ####http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplL4AvhrrOIv1fv2.nc?analysed_sst[(1981-09-01):1:(2015-04-05T00:00:00Z)][(10):1:(60)][(-150):1:(-100)]
  print(startdate)
  f = CFILE(filenm,mode="wb")
  curlPerform(url=url,writedata=f@ref) 
  close(f)
    i<-i+1
  if (is.na(file.info(filenm)$size)) {
    i<-i-1
  }
  else if (file.info(filenm)$size < 2000){
    i<-i-1
  }
}




#### SD

#setwd("~/Dropbox/Transfer/Work laptop/bight")

ptm = proc.time()

#sstnc = open.ncdf("sst.nc",write=TRUE)
filenm<-"sst.nc"
filenm<-"etopo180_48d2_23d2_fc23.nc"
#filenm<-"jplG1SST_2012_1_01_all.nc"
#filenm<-"nodcPH2sstd1day_1990_1_01_all.nc"

etoponc<-open.ncdf(filenm)
print(names(etoponc$var))
v1<-etoponc$var[[1]]
data3 <- get.var.ncdf(etoponc, "altitude")
lon=get.var.ncdf(etoponc,"longitude")
lat=get.var.ncdf(etoponc,"latitude")
rsd<-focalsd(data3,min(lon),max(lon),min(lat),max(lat),5,5)

b <- brick(filenm, lvar=3)
NAvalue(b) <- NA
plot(b) 
rasterDF <- raster(b)

#rsdregrid<-resample(rsd, rasterDF, method="bilinear")

rnc = writeRaster(rsd,filename="etopo-bight-sd.nc",format="CDF",overwrite=TRUE) # Write to netcdf file





filenames <- Sys.glob("nodcPH2*")

for(filenm in filenames){
  sstnc = open.ncdf(filenm)
  sstvals<-get.var.ncdf(sstnc)
  v1<-sstnc$var[[1]]
  varsize <- v1$varsize
  ndims <- v1$ndims
  nt <- varsize[ndims]
  lon=get.var.ncdf(sstnc,"longitude")
  lat=get.var.ncdf(sstnc,"latitude")

  for(i in 1:nt) {
    ptm = proc.time()

    # Initialize start and count to read one timestep of the variable.
    start <- rep(1,ndims) # begin with start=(1,1,1,...,1)
    start[ndims] <- i # change to start=(1,1,1,...,i) to read timestep i
    count <- varsize # begin w/count=(nx,ny,nz,...,nt), reads entire var
    count[ndims] <- 1 # change to count=(nx,ny,nz,...,1) to read 1 tstep
    data3 <- get.var.ncdf( sstnc, v1, start=start, count=count )
    # Now read in the value of the timelike dimension
    timeval <- get.var.ncdf( sstnc, v1$dim[[ndims]]$name, start=i, count=1 )
    print(paste("Data for variable",v1$name,"at timestep",i,
    " (date =",format(as.POSIXct(timeval, origin = "1970-01-01",tz="GMT")),"):"))
    #print(data3[1:10,1:10])

    rsd<-focalsd(data3,min(lon),max(lon),min(lat),max(lat),5,5)
    
    rsdregrid<-resample(rsd, rasterDF, method="bilinear")
    #xys<-cbind(factors$lon,factors$lat)
    #rsdvals<-extract(rsdregrid,spfactors)
    #factors<-cbind(factors,rsdvals)
   # names(factors)[length(factors)]<-paste("sstsd",as.Date(time),sep='-')
    #names(factors)[length(factors)]<-"sshsd"

    #b <- as(rsdregrid, "SpatialPixelsDataFrame")
    #b = SpatialPointsDataFrame(coordinates(b),data=b@data)
    #b2 = merge(factors,as.data.frame(b),by.x="lon", by.y="lat", all.x=TRUE, sort=FALSE)
    writetime<-(as.POSIXct(timeval, origin = "1970-01-01",tz="GMT"))

    rnc = writeRaster(rsd,filename=paste("./sd/nodcPH2-sd-",as.Date(writetime),".nc",sep=""),format="CDF",overwrite=TRUE) # Write to netcdf file
    rnc = writeRaster(rsdregrid,filename=paste("./sd/nodcPH2-sd-regrid",as.Date(writetime),".nc",sep=""),format="CDF",overwrite=TRUE) # Write to netcdf file
    
    #run grd2xyz through blockmean to regrid to 0.25x0.25 resolution.
    #gmt.system("grd2xyz sstsd-working.nc",file="sstsd-working.xyz")
    #gmt.system("blockmean sstsd-working.xyz -Rd225/245/30/49 -I0.25/0.25", file="sstsd-blockmean.xyz")
    
    # uncomment to read in 0.25 deg blockmean file
    #sstsd = read.table("sstsd-blockmean.xyz") # This is the 0.25x0.25 deg SD
    
    # Assuming here you want to make NetCDF file for reading back in to r2dtable
    #ncoutfile = paste("sstsd-",format(as.POSIXct(timeval, origin = "1970-01-01"),format="%Y-%m-%d"),".nc",sep="")
    #gmt.system(paste("xyz2grd sstsd-blockmean.xyz -Rd225/245/30/49 -I0.25/0.25 -G", ncoutfile,sep=""))
    #proc.time() - ptm
  }

  close.ncdf(sstnc)

  # Report the time elapsed for the loop
  proc.time() - ptm
}


filenames <- Sys.glob("jplG1*")

for(filenm in filenames){
  sstnc = open.ncdf(filenm)
  sstvals<-get.var.ncdf(sstnc)
  v1<-sstnc$var[[1]]
  varsize <- v1$varsize
  ndims <- v1$ndims
  nt <- varsize[ndims]
  lon=get.var.ncdf(sstnc,"longitude")
  lat=get.var.ncdf(sstnc,"latitude")

  for(i in 1:nt) {
    ptm = proc.time()

    # Initialize start and count to read one timestep of the variable.
    start <- rep(1,ndims) # begin with start=(1,1,1,...,1)
    start[ndims] <- i # change to start=(1,1,1,...,i) to read timestep i
    count <- varsize # begin w/count=(nx,ny,nz,...,nt), reads entire var
    count[ndims] <- 1 # change to count=(nx,ny,nz,...,1) to read 1 tstep
    data3 <- get.var.ncdf( sstnc, v1, start=start, count=count )
    # Now read in the value of the timelike dimension
    timeval <- get.var.ncdf( sstnc, v1$dim[[ndims]]$name, start=i, count=1 )
    print(paste("Data for variable",v1$name,"at timestep",i,
    " (date =",format(as.POSIXct(timeval, origin = "1970-01-01",tz="GMT")),"):"))
    #print(data3[1:10,1:10])

    rsd<-focalsd(data3,min(lon),max(lon),min(lat),max(lat),5,5)
    
    rsdregrid<-resample(rsd, rasterDF, method="bilinear")
    #xys<-cbind(factors$lon,factors$lat)
    #rsdvals<-extract(rsdregrid,spfactors)
    #factors<-cbind(factors,rsdvals)
   # names(factors)[length(factors)]<-paste("sstsd",as.Date(time),sep='-')
    #names(factors)[length(factors)]<-"sshsd"

    #b <- as(rsdregrid, "SpatialPixelsDataFrame")
    #b = SpatialPointsDataFrame(coordinates(b),data=b@data)
    #b2 = merge(factors,as.data.frame(b),by.x="lon", by.y="lat", all.x=TRUE, sort=FALSE)
    writetime<-(as.POSIXct(timeval, origin = "1970-01-01",tz="GMT"))

    rnc = writeRaster(rsd,filename=paste("./sd/jplG1-sd-",as.Date(writetime),".nc",sep=""),format="CDF",overwrite=TRUE) # Write to netcdf file
    rnc = writeRaster(rsdregrid,filename=paste("./sd/jplG1-sd-regrid",as.Date(writetime),".nc",sep=""),format="CDF",overwrite=TRUE) # Write to netcdf file
    
    #run grd2xyz through blockmean to regrid to 0.25x0.25 resolution.
    #gmt.system("grd2xyz sstsd-working.nc",file="sstsd-working.xyz")
    #gmt.system("blockmean sstsd-working.xyz -Rd225/245/30/49 -I0.25/0.25", file="sstsd-blockmean.xyz")
    
    # uncomment to read in 0.25 deg blockmean file
    #sstsd = read.table("sstsd-blockmean.xyz") # This is the 0.25x0.25 deg SD
    
    # Assuming here you want to make NetCDF file for reading back in to r2dtable
    #ncoutfile = paste("sstsd-",format(as.POSIXct(timeval, origin = "1970-01-01"),format="%Y-%m-%d"),".nc",sep="")
    #gmt.system(paste("xyz2grd sstsd-blockmean.xyz -Rd225/245/30/49 -I0.25/0.25 -G", ncoutfile,sep=""))
    #proc.time() - ptm
  }

  close.ncdf(sstnc)

  # Report the time elapsed for the loop
  proc.time() - ptm
}


### CHECK NCDF file
#startdate<-
#data <- open.ncdf(filenm)
