##Script to calculate and write the standard set of derived variables
##for the 800m data

source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R',chdir=T)
source('/home/ssobie/code/repos/downscale_CMIP6/bccaqv2.simplified.climdex.support.r')
source('/home/ssobie/code/repos/downscale_CMIP6/canada.bccaq2.derived.files.r')
source('/home/ssobie/code/repos/downscale_CMIP6/climdex.variables.functions.r')

library(ncdf4)
library(PCICt)
library(climdex.pcic)
library(foreach)
library(doParallel)
registerDoParallel(cores=2) # or some other number that you're comfortable with.

##--------------------------------------------------------------
##****************************************************************
testing <- FALSE

if (testing) {
   tmpdir <- '/local_temp/ssobie'
   gcm <- 'CanESM5'
   scenario <- 'ssp585'
   run <- 'r1i1p2f1'
   type <- 'r9'
   climname <- 'r95'
} else {
   args <- commandArgs(trailingOnly=TRUE)
   for(i in 1:length(args)){
       eval(parse(text=args[[i]]))
   }
}

varname <- input.varname[[climname]]
climdex.name <- paste0('climdex.',climname)
climdex.info <- get.climdex.info(climdex.name)

tmp.dir <- paste0(tmpdir,'/',gcm,'_',scenario,'_',run,'_climdex_',type,'_',varname,'/')
if (!file.exists(tmp.dir)) {
  dir.create(tmp.dir,recursive=TRUE)
}

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
gcm.file <- paste0(varname,'_day_',gcm,'_North_America_historical+',scenario,'_',run,'_gn_19500101-21001231.nc')
file.copy(from=paste0(gcm.dir,gcm.file),to=tmp.dir)
print('Done copying gcm file')
gcm.tmp <- paste0(tmp.dir,gcm.file)

input.dir <- paste0(data.dir,'ds_',varname,'_',gcm,'_',run,'_',scenario,'_split15/')
split.files <- list.files(path=input.dir,pattern=varname)
slen <- length(split.files)
input.file <- split.files[1]
input.tmp <- paste0(tmp.dir,'/',input.file)
file.copy(from=paste0(input.dir,input.file),to=tmp.dir,overwrite=TRUE)


##---------------------------------------------------------------------------
if (type=='annual') {
  ##Annual Average Files for writing
  print('Ann Avg opening')
  print('Ann Avg opening')
  out.dir <- paste0(tmp.dir,'climdex/')
  dir.create(paste0(write.dir,'climdex/'),recursive=TRUE,showWarnings=FALSE)
  ann.file <-  make_climdex_file(climname,gcm,scenario,run,
                input.tmp,obs.tmp,gcm.tmp,
                data.dir,out.dir,tmp.dir)
  ann.ncs <- nc_open(paste0(out.dir,ann.file),write=TRUE)
  common.lat <- ncvar_get(ann.ncs,'lat')
}

##---------------------------------------------------------------------------
if (type=='monthly') {
  ##Monthly Average Files for writing
  print('Monthly avg opening')

  out.dir <- paste0(tmp.dir,'climdex/')
  dir.create(paste0(write.dir,'climdex/'),recursive=TRUE,showWarnings=FALSE)
  mon.file <- make_climdex_file(climname,gcm,scenario,run,
                                input.tmp,obs.tmp,gcm.tmp,
                                data.dir,out.dir,tmp.dir)
  mon.ncs <- nc_open(paste0(out.dir,mon.file),write=TRUE)
  common.lat <- ncvar_get(mon.ncs,'lat')
}

if (type=='r9') {
  ##R95 Files for writing
  print('R9 opening')
  ann.names <- c(paste0(climname,'p'),
                 paste0(climname,'days'),
                 paste0(climname,'store'))
  ann.ncs <- vector(mode='list',length=length(ann.names))
  ann.files <- rep('A',length(ann.names))
  out.dir <- paste0(tmp.dir,'climdex/')
  dir.create(paste0(write.dir,'climdex/'),recursive=TRUE,showWarnings=FALSE)

  for (d in seq_along(ann.names)) {
     ann.files[d] <- make_climdex_file(ann.names[d],gcm,scenario,run,
                                   input.tmp,obs.tmp,gcm.tmp,
                                   data.dir,out.dir,tmp.dir) 
     ann.ncs[[d]] <- nc_open(paste0(out.dir,ann.files[d]),write=TRUE)  
  }
  names(ann.ncs) <- c('total','days','store')
  common.lat <- ncvar_get(ann.ncs[[1]],'lat')
}



##---------------------------------------------------------------------------
##---------------------------------------------------------------------------

##Iterate over the latitude files
for (i in 1:slen) {
   print(paste0('Lat band ',i,' of ',slen))
   input.file <- split.files[i]
   file.copy(paste0(input.dir,"/",input.file),tmp.dir,overwrite=TRUE)

   print('Data opening')
   input.nc <- nc_open(paste0(tmp.dir,input.file),write=FALSE)
   input.dates <- netcdf.calendar(input.nc)
   yearly.fac <- as.factor(format(input.dates,'%Y'))
   monthly.fac <- as.factor(format(input.dates,'%Y-%m'))  

   lon <- ncvar_get(input.nc,'lon')
   lat <- ncvar_get(input.nc,'lat')
   n.lon <- length(lon)
   n.lat <- length(lat)
   lat.match <- which(common.lat %in% lat)
   lat.bnds <- c(lat.match[1],(tail(lat.match,1)-lat.match[1])+1)

   print('Latitude bands:')
   print(lat.bnds)

   for (j in 1:n.lat) { ##n.lon) {
     lat.ix <- lat.match[j]

     print(paste0('Latitude: ',j,' of ',n.lat))
     input.subset <- ncvar_get(input.nc,varname,start=c(1,j,1),count=c(-1,1,-1))
     flag <- is.na(input.subset[,1])
     input.list <- vector(mode='list',length=n.lon)

     input.list <- lapply(seq_len(nrow(input.subset)), function(k) input.subset[k,])
     rm(input.subset)

    ##----------------------------------------------------------
    ##Annual Averages 
    if (type=='annual') {
       annual_climdex_for_model(climname,ann.ncs,lat.ix,n.lon,yearly.fac,flag,
                                input.list)
    }
    ##----------------------------------------------------------
    ##Monthly Averages 
    if (type=='monthly') {
       monthly_climdex_for_model(climname,mon.ncs,lat.ix,n.lon,monthly.fac,flag,
                                 input.list)
    }
    ##----------------------------------------------------------
    ##Extreme Precip
    if (type=='r9') {
       r9_precip_for_model(climname,ann.names,ann.ncs,lat.ix,n.lon,yearly.fac,input.dates,flag,
                           input.list)
    }

    rm(input.list)
  }##Longitude Loop
  nc_close(input.nc)

  print('Removing lat band files')
  file.remove(paste0(tmp.dir,"/",input.file))

}##Latitude File Loop


if (type=='annual') {
   nc_close(ann.ncs)
   file.copy(from=paste0(out.dir,ann.file),to=paste0(write.dir,'climdex/'),overwrite=TRUE)
}

if (type=='monthly') {
  nc_close(mon.ncs)
  file.copy(from=paste0(out.dir,mon.file),to=paste0(write.dir,'climdex/'),overwrite=TRUE)
}

if (type=='r9') {
  for (d in seq_along(ann.names)) {
    nc_close(ann.ncs[[d]])  
    file.copy(from=paste0(out.dir,ann.files[d]),to=paste0(write.dir,'climdex/'),overwrite=TRUE)
  }
}

file.remove(obs.tmp)
file.remove(gcm.tmp)
