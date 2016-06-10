# $Id$
# simulate correlated random walk (CRW) from empirical distribution of turn angles and speed in tag data

# require packages
require(adehabitat) # help(package='adehabitat') # help.search('angle',package='adehabitat')
require(maps)       # for map.where
require(mapdata)    # for worldHires
require(sp)
require(maptools)
require(gdata)

# setup paths
##wd = 'D:/code/tortugas/code'; setwd(wd)
##wd = 'E:/code/tortugas/code'
#wd = 'C:/dana/tortugas/code'
#in.dir = sprintf('%s/../data/yanns_data', wd)
#in.csv = sprintf('%s/all_tags/indiv_tags/nonTOPP/nonTOPP_tags_yt_interp_valid_funky5.csv', in.dir)
#out.dir = sprintf('%s/../data/crw_data/crw_output_data/nonTOPP_crws', wd)
##out.alltags.csv = sprintf('%s/nonTOPP_all.csv', out.dir)

# NEW (4ssm)
  wd = '~/Dropbox/Shared Folders/DOM_dl/LBST'
#  wd = '/Volumes/iDisk/Documents/R/MolaMola/'
  in.dir = sprintf('%s', wd)                   # NEW (4ssm)
  in.csv = sprintf('%sBlueWhale_SSM.csv', in.dir)                                                         # NEW (4ssm)
  out.dir = sprintf('%s/CRW_output', wd)                             # NEW (4ssm)


# read in tag data
colnames<-c(names(read.csv("~/Dropbox/Shared Folders/DOM_dl/LBST/CCA2011F40672F.csv",head=TRUE)),"day","month","year")
colnames[3]<-"lat"; colnames[4]<-"lon"
txt_files = list.files(path=wd)
txt_files = txt_files[grep(".xls",txt_files)]
setwd(wd)
data_list = lapply(txt_files, read.xls, sep = ",", head=FALSE)
setwd(wd)
latvect<-data_list[[1]]
colnames(latvect)<-colnames
colnames(latvect)[1]<-"ID"
#latvect$ID<-txt_files[1]
#latvect<-cbind(latvect$ID,latvect[,1:4])
#names(latvect)[1]<-"ID"

for (filenum in 2:length(txt_files)) {
	tempvect<-data_list[[filenum]][,1:length(colnames)]
	colnames(tempvect)<-colnames
	colnames(tempvect)[1]<-"ID"
	latvect<-rbind(latvect,tempvect)	
}

tags<-latvect
#tags$lon[tags$lon>0]<-tags$lon[tags$lon>0]-360


#tags$date <- paste(gsub(" ","",paste(tags$Month,tags$Day,tags$Year,sep="/"), fixed=TRUE), " 12:00:00 GMT")
#tags$dTime = as.POSIXct(strptime(as.character(tags$date), "%m/%d/%Y %H:%M", tz="GMT"))
tags$date <- paste(gsub(" ","",paste(tags$month,tags$day,tags$year,sep="/"), fixed=TRUE), " 12:00:00 GMT")
tags$dTime = as.POSIXct(strptime(as.character(tags$date), "%m/%d/%Y %H:%M", tz="GMT"))
tags$tag<-tags$ID
#clean up tags for Molas
#tags[tags$lon>110&tags$lon<120,]$lon<--1*tags[tags$lon>110&tags$lon<120,]$lon
#tags<-tags[tags$lon<110,]
#tags<-tags[!tags$ptt== 41756,] ## only one data point
#tags<-tags[!tags$ptt== 52918,] ## only one data point
#tags<-tags[!tags$ptt== 52943,] ## only one data point
#tags<-tags[!tags$ptt== 61918,] ## only one data point
#tags<-tags[!tags$ptt== 61923,] ## only one data point
#tags<-tags[!tags$ptt== 64270,] ## only one data point
#tags<-tags[!tags$ptt== 88766,] ## only one data point

# note: you can read *.mat files directly
#require(R.matlab)     # install this package by adding Omegahat through Packages > Selecting repositories
#dat = readMat('E:/code/tortugas/data/yanns_data/all_tags/16264.mat')

# note: you can iterate through all files in a directory like:
#in.dir = sprintf('%s/../data/tag',wd)
#for (in.csv in list.files(in.dir, glob2rx('*SPOT4.csv'), full.names=T){
#  tags = read.csv(in.csv)
#  #...
#}

# clear sim.alltags if exists in workspace
#if (exists('sim.alltags')) rm(sim.alltags)

#tags to be run:
#tagsleft<-unique(tags$eventid)[7:64]

for (tagid in unique(tags$tag)[16:length(unique(tags$tag))]){    #CAN SUBSET TAGS USING: metad[metad[,1]>510241600,1]

#for (tagid in unique(tags$ptt)[14:17]){    #starting with 66884
# clear sim.alltags if exists in workspace
if (exists('sim.alltags')) rm(sim.alltags)

# run code for indiv tags...
# tag ids to choose from:   41749 41752 41756 41767 52918 52943 61918 61919 61920 61922 61923 61924 63978 63980 63983 64264 64270 65858 77160 88766 89297 91973
# NEW tag ids to choose from:   41749 41752 41767 61918 61919 61920 61922 61923 61924 63978 63980 63983 64270 66884 77160 89297 91973
  # get indiv tag
  #tagid = 89297 # tracks done: 
  
  print(tagid)
  tag = tags[which(tags$tag==tagid),c('lon', 'lat','dTime')]
  if(sum(tag$lon>0)>0) tag$lon[tag$lon>0]<-tag$lon[tag$lon>0]-360

#  out.alltags.csv = sprintf('%s/crw_sim_%d.csv', out.dir,toString(tagid))
  out.alltags.csv = paste(out.dir,'/crw_sim_',toString(tagid),'.csv',sep='')

  # remove non-unique dates
  dupes = which(duplicated(tag$dTime))
  if (length(dupes)!=0){
    tag = tag[-dupes,]
  }

  # remove NAs
    tag = tag[!is.na(tag$lat),]

  # restrict to NE Pacific
    tag = tag[tag$lat>25&tag$lon>-140,]

  # Creation of an object of class "ltraj"
  tr = as.ltraj(cbind(tag$lon, tag$lat), date=tag$dTime, id=tagid)
  tr1 = tr[[1]]
  #tr1<-tr1[1:(which(tr1$dist>1)-1),]

  head(tr1)
  #x11()
  plot(tr)
      
  # get indices to rows of trajectory that are not NA for sampling in simulation
  i.tr1.nona = which(!is.na(tr1[['dist']]) & !is.na(tr1[['rel.angle']]))
  #tr1<-tr1[i.tr1.nona,]
  # setup map per tag
  #mex.map = map('worldHires', 'Gabon', fill=T, col='transparent', plot=F)
  CC.map = map('worldHires', fill=T, col='transparent', plot=F, ylim = c(20,60))

  CC.IDs = sapply(strsplit(CC.map$names, ":"), function(x) x[1])
  CC.sp = map2SpatialPolygons(CC.map, IDs=CC.IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

  # setup data frame for simulation
  n.tag = nrow(tag)
  #n.sim = n.tag # or change this to 1000, or n.tag+1000
  n.sim = 200 # or change this to 1000, or n.tag+1000
  sim = data.frame(x=numeric(n.sim), y=numeric(n.sim), t=as.POSIXct(rep(NA,n.sim)), tagid=rep(tagid,n.sim), iteration=1:n.sim)
  #sim = data.frame(x=numeric(n.sim), y=numeric(n.sim), t=as.POSIXct(rep(NA,n.sim)),iteration=1:n.sim)

  if(n.tag > 0){
  for (k in 1:n.sim){

  print(sprintf("  k'th simulation: %d",k))
  sim = data.frame(x=numeric(n.tag), y=numeric(n.tag), t=as.POSIXct(rep(NA,n.tag)), flag=rep(NA,n.tag))     # new location

# create column to keep track of sim iteration number
  sim$iteration = rep(k, n.tag)

  # populate initial location
  sim[1,'x'] = tag$lon[1]
  sim[1,'y'] = tag$lat[1]
  sim[1,'t'] = tr1$date[1]
  angle      = tr1[2,'abs.angle'] # starting angle
  dtime      = tr1[1,'date']      # starting time

  #debug plot check
  #par(mar=c(3.5, 2, 1, 1)) # margin = c(bottom, left, top, right)
  map('worldHires', xlim=c(-145,-115), ylim=c(20,50))         # Bluewhale-centric projection
#  map('worldHires', xlim=c(-100,5), ylim=c(18,50))         # atlantic-centric projection
  map.axes()
  lines(tag$lon, tag$lat,col='grey')                          # plot grey lines for original track
  points(sim[1,'x'],sim[1,'y'], col='blue', pch=2, cex=2)     # plot initial point like ltraj symbology
  title(main = sprintf("%d 'th CRW Simulation for tagid %s", k,tagid))


# for (j in 2:n.sim){ # j = 2
  for (j in 2:n.tag){ # j = 2
    on.land = T # start with assumption on land to force getting new location
    while (on.land == T){
      # get random index of trajectory row, outside NA values
      i = sample(i.tr1.nona, 1)

      # get distance and angle from original data
      dist  = tr1[i,'dist']
      angle = angle + tr1[i,'rel.angle']

	  # force westward if outside of gulf
#	  if (tr1[i,"x"]<-80) {angle<-runif(1,3.14,5.05)}

      # calculate a new x and y
      x = sim[j-1,'x'] + dist * cos(angle)
      y = sim[j-1,'y'] + dist * sin(angle)

      # check if on land
      pt = SpatialPoints(matrix(c(x,y),nrow=1),proj4string=CRS("+proj=longlat +datum=WGS84"))
      place = over(pt, CC.sp)

      #debug plot check
      if (is.na(place)){
        # assign valid on.water location to sim data frame
        sim[j,'x'] = x
        sim[j,'y'] = y
        sim[j,'t'] = sim[j-1,'t'] + tr1[i,'dt']

        # update plot with points
        lines(sim[(j-1):j, c('x','y')], col='black')
        points(x, y, col='blue', pch=20)

        # set to F so bumps out of while loop and to next simulated location
        on.land = F
      } else {
        points(x, y, col='red', pch=20)
      }
    } # end while (on.land == T){

  } # end for (j in 2:n.sim)

#### Calculate flag for quality of pseudo-track
  tagstart<-rbind(tag[1,],tag[2,],tag[n.tag,])
  tagsim<-rbind(sim[1,],sim[2,],sim[n.tag,])
  tagstart$lat[2]<-tagstart$lat[1]-0.01
  tagsim$y[2]<-tagsim$y[1]-0.01
  trstart = as.ltraj(cbind(tagstart$lon, tagstart$lat), date=tagstart$dTime,id=tagid)
  trsim = as.ltraj(cbind(tagsim$x, tagsim$y), date=tagsim$t,id=tagid)
  # trstart[[1]]$rel.angle[2]*180/pi
  distdiff<-abs(trstart[[1]]$dist[2]-trsim[[1]]$dist[2])
  angdiff<-abs(trstart[[1]]$rel.angle[2]*180/pi-trsim[[1]]$rel.angle[2]*180/pi)
  sim$flag<-distdiff/trstart[[1]]$dist[2]*3+angdiff/45
#if (distdiff/trstart[[1]]$dist[2]<(1/3)) sim$flag<-1  
#if (distdiff/trstart[[1]]$dist[2]<(2/3)) sim$flag<-2
#if (distdiff/trstart[[1]]$dist[2]<(3/3)) sim$flag<-3
#if (angdiff < 45 | angdiff >315) sim$flag<-sim$flag
#if (angdiff < 90 & angdiff >45) sim$flag<-sim$flag+1
#if (angdiff > 90 & angdiff <135) sim$flag<-sim$flag+2
#if (angdiff < 180 & angdiff >135) sim$flag<-sim$flag+3
#if (angdiff > 180 & angdiff <225) sim$flag<-sim$flag+3
#if (angdiff < 270 & angdiff >225) sim$flag<-sim$flag+2
#if (angdiff < 315 & angdiff >270) sim$flag<-sim$flag+1

#  head(trstart[[1]])
#  head(trsim[[1]])


  # append full tag simulation to sim.alltags
  if (exists('sim.alltags')){
    sim.alltags = rbind(sim.alltags, sim)
  } else {
    sim.alltags = sim
  }

 } # end for (k in 2:n.sim)
 } # end if n.tag > 0
  # write out csv per tag
  #out.csv = sprintf('%s/crw_sim_%d.csv', out.dir, tagid)
  #write.csv(sim, file=out.csv, row.names=F)

  # write out
write.csv(sim.alltags, out.alltags.csv, row.names=F)      #OLD (commented out 4ssm)

 ################################################################
  # output map
  #out.png = sprintf('%s/crw_sim_%d.png', out.dir, tagid); res = 300 # resolution in dots per inch
  out.png = paste(out.dir,'/crw_sim_',toString(tagid),'.png',sep=''); res = 300 # resolution in dots per inch

  dev.print(device=png, file=out.png, width=8*res, height=8*res, res=res)

}  # end for (tagid in unique(tags$ptt)){



# write out
#write.csv(sim.alltags, out.alltags.csv, row.names=F)



