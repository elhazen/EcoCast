getZ <- function(inpts,rastIn){
  rastIn <- raster(rastIn)
  inpts2 <- inpts
  coordinates(inpts2) <- c('lon','lat')
  proj4string(inpts2) <- CRS("+proj=longlat +ellps=WGS84")
  z <- extract(rastIn,inpts2@coords,method='simple',na.rm=F,cellnumbers=F)
  EOVout <- cbind(inpts,z)
  rm(inpts2,inpts); return(EOVout)
}

getZsd <- function(inpts,rastIn){
  rastIn <- raster(rastIn)
  inpts2 <- inpts
  coordinates(inpts2) <- c('lon','lat')
  proj4string(inpts2) <- CRS("+proj=longlat +ellps=WGS84")
  zsd <- extract(rastIn,inpts2@coords,method='simple',na.rm=F,cellnumbers=F)
  EOVout <- cbind(inpts,zsd)
  rm(inpts2,inpts); return(EOVout)
}

xy$z <- c()
xy$zsd <- c()
rastIn <- '~/Dropbox/Shared Folders/DOM_dl/ETOPO1/ETOPO1_Bed_g_geotiff.tif'
xy <- getZ(inpts=xy,rastIn=rastIn)
rastIn <- '~/Dropbox/Shared Folders/DOM_dl/ETOPO1/ETOPO1_CCS_bathySD.grd'
xy <- getZsd(inpts=xy,rastIn=rastIn)