##Script to calculate and write the standard set of derived variables
##for the 800m data

source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R',chdir=T)
source('/home/ssobie/code/repos/downscale_CMIP6/bccaqv2.derived.variable.support.r')
source('/home/ssobie/code/repos/downscale_CMIP6/canada.bccaq2.derived.files.r')
source('/home/ssobie/code/repos/downscale_CMIP6/degree.day.variables.functions.r')

library(ncdf4)
library(PCICt)
library(foreach)
library(climdex.pcic)

##--------------------------------------------------------------
##****************************************************************
testing <- FALSE


if (testing) {
   tmpdir <- '/local_temp/ssobie'
   gcm <- 'CanESM5'
   scenario <- 'ssp585'
   run <- 'r1i1p2f1'
   type <- 'gsl'

} else {
   args <- commandArgs(trailingOnly=TRUE)
   for(i in 1:length(args)){
       eval(parse(text=args[[i]]))
   }
}

tmp.dir <- paste0(tmpdir,'/',gcm,'_',scenario,'_',run,'_',type,'/')
if (!file.exists(tmp.dir)) {
  dir.create(tmp.dir,recursive=TRUE)
}

degree.names <- c('cdd','fdd','gdd','hdd')

base.dir <- '/storage/data/climate/downscale/BCCAQ2/'
data.dir <- paste0(base.dir,'raw_downscaled/')
write.dir <- paste0(base.dir,'CMIP6_BCCAQv2/',gcm,'/Derived/',gcm,'_',scenario,'_',run,'/')
if (!file.exists(write.dir))
  dir.create(write.dir,recursive=TRUE)

##Transfer template files for derived file creation
print('Transfer Obs File')
obs.dir <- '/storage/data/climate/observations/gridded/ANUSPLIN/ANUSPLIN_300ARCSEC/'
obs.file <- 'anusplin_template_one.nc'
file.copy(from=paste0(obs.dir,obs.file),to=tmp.dir)
obs.tmp <- paste0(tmp.dir,obs.file)

gcm.dir <- paste0('/storage/data/climate/CMIP6/assembled/',gcm,'/north_america/')
gcm.file <- paste0('tasmax_day_',gcm,'_North_America_historical+',scenario,'_',run,'_gn_19500101-21001231.nc')
file.copy(from=paste0(gcm.dir,gcm.file),to=tmp.dir)
print('Done copying gcm file')
gcm.tmp <- paste0(tmp.dir,gcm.file)

tasmax.dir <- paste0(data.dir,'ds_tasmax_',gcm,'_',run,'_',scenario,'_split15/')
tasmax.split.files <- list.files(path=tasmax.dir,pattern='tasmax')
tasmin.dir <- paste0(data.dir,'ds_tasmin_',gcm,'_',run,'_',scenario,'_split15/')
tasmin.split.files <- list.files(path=tasmin.dir,pattern='tasmin')

slen <- length(tasmax.split.files)
tasmax.file <- tasmax.split.files[1]
tasmax.tmp <- paste0(tmp.dir,'/',tasmax.file)
file.copy(from=paste0(tasmax.dir,tasmax.file),to=tmp.dir,overwrite=TRUE)

if (type=='degree_days') {
  print('Degree days opening')
  out.dir <- paste0(tmp.dir,'degree_days/')
  dir.create(paste0(write.dir,'degree_days/'),recursive=TRUE,showWarnings=FALSE)
  dd.files <-  make_degree_day_files(degree.names,gcm,scenario,run,
                                     tasmax.tmp,obs.tmp,gcm.tmp,
                                     data.dir,out.dir,tmp.dir)
  dd.ncs <- vector(mode='list',length=length(dd.files))
  for (d in seq_along(dd.files)) {
    dd.ncs[[d]] <- nc_open(paste0(out.dir,dd.files[d]),write=TRUE)
  }
  common.lat <- ncvar_get(dd.ncs[[1]],'lat')
  tas <- TRUE
  tasdiff <- FALSE

}

if (type=='gsl') {

  out.dir <- paste0(tmp.dir,'climdex/')
  dir.create(paste0(write.dir,'climdex/'),recursive=TRUE,showWarnings=FALSE)
  gsl.file <-  make_climdex_file('gsl',gcm,scenario,run,
                                 tasmax.tmp,obs.tmp,gcm.tmp,
                                 data.dir,out.dir,tmp.dir)
  gsl.ncs <- nc_open(paste0(out.dir,gsl.file),write=TRUE)
  common.lat <- ncvar_get(gsl.ncs,'lat')
  tas <- TRUE
  tasdiff <- FALSE

}

if (type=='dtr') {
  out.dir <- paste0(tmp.dir,'climdex/')
  dir.create(paste0(write.dir,'climdex/'),recursive=TRUE,showWarnings=FALSE)
  dtr.file <-  make_climdex_file('dtr',gcm,scenario,run,
                                 tasmax.tmp,obs.tmp,gcm.tmp,
                                 data.dir,out.dir,tmp.dir)
  dtr.ncs <- nc_open(paste0(out.dir,dtr.file),write=TRUE)
  common.lat <- ncvar_get(dtr.ncs,'lat')
  tas <- FALSE
  tasdiff <- TRUE
}



##---------------------------------------------------------------------------

##Iterate over the split files
for (i in 1:slen) {
  print(paste0('Lat band ',i,' of ',slen))
  print('TX Opening')
  tasmax.file <- tasmax.split.files[i]
  file.copy(paste0(tasmax.dir,"/",tasmax.file),tmp.dir,overwrite=TRUE)
  tasmax.nc <- nc_open(paste0(tmp.dir,tasmax.file),write=FALSE)
  tasmax.dates <- netcdf.calendar(tasmax.nc)
  yearly.fac <- as.factor(format(tasmax.dates,'%Y'))
  monthly.fac <- as.factor(format(tasmax.dates,'%Y-%m'))

  print('TN Opening')
  tasmin.file <- tasmin.split.files[i]
  file.copy(paste0(tasmin.dir,"/",tasmin.file),tmp.dir,overwrite=TRUE)
  tasmin.nc <- nc_open(paste0(tmp.dir,tasmin.file),write=FALSE)

  lon <- ncvar_get(tasmax.nc,'lon')
  lat <- ncvar_get(tasmax.nc,'lat')
  n.lon <- length(lon)
  n.lat <- length(lat)
  lat.match <- which(common.lat %in% lat)
  lat.bnds <- c(lat.match[1],(tail(lat.match,1)-lat.match[1])+1)

  print('Latitude bands:')
  print(lat.bnds)

  for (j in 1:n.lat) { ##n.lon) {
    lat.ix <- lat.match[j]
    ltm <- proc.time()

    print(paste0('Latitude: ',j,' of ',n.lat))
    tasmax.subset <- ncvar_get(tasmax.nc,'tasmax',start=c(1,j,1),count=c(-1,1,-1))
    tasmin.subset <- ncvar_get(tasmin.nc,'tasmin',start=c(1,j,1),count=c(-1,1,-1))    
    if (tas) {
      input.subset <- (tasmax.subset + tasmin.subset)/2
      input.list <- vector(mode='list',length=n.lon)
      input.list <- lapply(seq_len(nrow(input.subset)), function(k) input.subset[k,])
    }
    if (tasdiff) {
       input.subset <- tasmax.subset - tasmin.subset
       input.list <- vector(mode='list',length=n.lon)
       input.list <- lapply(seq_len(nrow(input.subset)), function(k) input.subset[k,])
    }
    flag <- is.na(input.subset[,1])
    rm(input.subset)
    rm(tasmax.subset)
    rm(tasmin.subset)

    ##----------------------------------------------------------
    if (type=='degree_days') {
      ##Degree Day 
      degree_days_for_model(degree.names,dd.ncs,lat.ix,n.lon,flag,
                            input.list,yearly.fac)
    }
    ##----------------------------------------------------------
    ##GSL
    if (type=='gsl') {
      gsl_for_model('gsl',gsl.ncs,lat.ix,n.lon,flag,
                    input.list,yearly.fac,tasmax.dates)
    }
 
    ##----------------------------------------------------------
    ##DTR 
    if (type=='dtr') {
      dtr_for_model('dtr',dtr.ncs,lat.ix,n.lon,flag,
                    input.list,monthly.fac)
    }
  }##Longitude Loop
  nc_close(tasmax.nc)
  nc_close(tasmin.nc)

  print('Removing lat band files')
  file.remove(paste0(tmp.dir,"/",tasmax.file))
  file.remove(paste0(tmp.dir,"/",tasmin.file))

}##Latitude File Loop

##Move back

if (type=='degree_days') {
  for (d in seq_along(degree.names)) {
    nc_close(dd.ncs[[d]]) 
    file.copy(from=paste0(out.dir,dd.files[d]),to=paste0(write.dir,'degree_days/'),overwrite=TRUE)
  }
}

if (type=='gsl') {
  nc_close(gsl.ncs) 
  file.copy(from=paste0(out.dir,gsl.file),to=paste0(write.dir,'climdex/'),overwrite=TRUE)
}

if (type=='dtr') {
  nc_close(dtr.ncs) 
  file.copy(from=paste0(out.dir,dtr.file),to=paste0(write.dir,'climdex/'),overwrite=TRUE)
}

file.remove(obs.tmp)
file.remove(gcm.tmp)



