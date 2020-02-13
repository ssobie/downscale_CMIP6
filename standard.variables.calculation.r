##Script to calculate the standard derived variables from the 
##split CMIP6 downscaled files

source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R',chdir=T)
source('/home/ssobie/code/repos/downscale_CMIP6/bccaqv2.derived.variable.support.r')
source('/home/ssobie/code/repos/downscale_CMIP6/canada.bccaq2.derived.files.r')
source('/home/ssobie/code/repos/downscale_CMIP6/standard.variables.functions.r')

library(ncdf4)
library(PCICt)
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
   type <- 'annual_extremes'
   varname <- 'tasmax'
   pctl <- '996'
} else {
   args <- commandArgs(trailingOnly=TRUE)
   for(i in 1:length(args)){
       eval(parse(text=args[[i]]))
   }
}



tmp.dir <- paste0(tmpdir,'/',gcm,'_',scenario,'_',run,'_',type,'_',pctl,'_',varname,'/')
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
##Annual Averages

if (type=='annual') {

  print('Ann Avg opening')
  out.dir <- paste0(tmp.dir,'annual/')
  dir.create(paste0(write.dir,'annual/'),recursive=TRUE,showWarnings=FALSE)
  ann.file <-  make_annual_file(varname,gcm,scenario,run,
                                input.tmp,obs.tmp,gcm.tmp,
                                data.dir,out.dir,tmp.dir)
  ann.ncs <- nc_open(paste0(out.dir,ann.file),write=TRUE)  
  common.lat <- ncvar_get(ann.ncs,'lat')

}

##---------------------------------------------------------------------------
##Annual Block Maxima Files for writing

if (type=='annual_extremes') {

  print('Ann extremes opening')
  out.dir <- paste0(tmp.dir,'annual_extremes/')
  dir.create(paste0(write.dir,'annual_extremes/'),recursive=TRUE,showWarnings=FALSE)
  ext.file <- make_return_period_file(varname,gcm,scenario,run,
                                      input.tmp,obs.tmp,gcm.tmp,
                                      data.dir,out.dir,tmp.dir)
  ext.ncs <- nc_open(paste0(out.dir,ext.file),write=TRUE)  
  common.lat <- ncvar_get(ext.ncs,'lat')

}

##---------------------------------------------------------------------------
##Annual Quantile Files for writing

if (type == 'annual_quantiles') {

  print('Ann quantiles opening')
  out.dir <- paste0(tmp.dir,'annual_quantiles/')
  dir.create(paste0(write.dir,'annual_quantiles/'),recursive=TRUE,showWarnings=FALSE)
  quant.file <- make_quantile_file(varname,gcm,scenario,run,pctl,
                                    input.tmp,obs.tmp,gcm.tmp,
                                    data.dir,out.dir,tmp.dir)

  quant.ncs <- nc_open(paste0(out.dir,quant.file),write=TRUE)
  common.lat <- ncvar_get(quant.ncs,'lat')
}

##---------------------------------------------------------------------------
##Seasonal Average Files for writing

if (type=='seasonal') {
  print('Seasonal averages opening')
  out.dir <- paste0(tmp.dir,'seasonal/')
  dir.create(paste0(write.dir,'seasonal/'),recursive=TRUE,showWarnings=FALSE)
  seas.file <- make_seasonal_file(varname,gcm,scenario,run,
                                  input.tmp,obs.tmp,gcm.tmp,
                                  data.dir,out.dir,tmp.dir)
  seas.ncs <- nc_open(paste0(out.dir,seas.file),write=TRUE)
  common.lat <- ncvar_get(seas.ncs,'lat')
}

##---------------------------------------------------------------------------
##Monthly Average Files for writing

if (type=='monthly') {

  print('monthly avg opening')
  out.dir <- paste0(tmp.dir,'monthly/')
  dir.create(paste0(write.dir,'monthly/'),recursive=TRUE,showWarnings=FALSE)
  mon.file <- make_monthly_file(varname,gcm,scenario,run,
                                input.tmp,obs.tmp,gcm.tmp,
                                data.dir,out.dir,tmp.dir)
  mon.ncs <- nc_open(paste0(out.dir,mon.file),write=TRUE)
  common.lat <- ncvar_get(mon.ncs,'lat')
}

##---------------------------------------------------------------------------
##---------------------------------------------------------------------------

##Iterate over the split files
for (i in 1:slen) {
  print(paste0('Lat band ',i,' of ',slen))
  input.file <- split.files[i]
  file.copy(paste0(input.dir,"/",input.file),tmp.dir,overwrite=TRUE)
  
  print('Data opening')
  input.nc <- nc_open(paste0(tmp.dir,input.file),write=FALSE)
  input.dates <- netcdf.calendar(input.nc)
  yearly.fac <- as.factor(format(input.dates,'%Y'))
  seasonal.fac <- get_seasonal_fac(input.dates)
  monthly.fac <- as.factor(format(input.dates,'%Y-%m'))  

  lon <- ncvar_get(input.nc,'lon')
  lat <- ncvar_get(input.nc,'lat')
  n.lon <- length(lon)
  n.lat <- length(lat)
  lat.match <- which(common.lat %in% lat)
  lat.bnds <- c(lat.match[1],(tail(lat.match,1)-lat.match[1])+1)

  print('Latitude bands:')
  print(lat.bnds)

  for (j in 1:n.lat) {
    lat.ix <- lat.match[j]
    ltm <- proc.time()

    print(paste0('Latitude: ',j,' of ',n.lat))
    input.subset <- ncvar_get(input.nc,varname,start=c(1,j,1),count=c(-1,1,-1))
    flag <- is.na(input.subset[,1])
    input.list <- vector(mode='list',length=n.lon)
    input.list <- lapply(seq_len(nrow(input.subset)), function(k) input.subset[k,])
    rm(input.subset)
    print('Lapply convert to list')

    ##----------------------------------------------------------
    ##Annual Averages 
    if (type=='annual') {
      annual_averages_for_model(varname,ann.ncs,lat.ix,n.lon,yearly.fac,flag,
                                input.list)
      print('Annual Averages Done')
    }
    ##----------------------------------------------------------
    ##Seasonal Averages 
    if (type=='seasonal') {
      seasonal_averages_for_model(varname,seas.ncs,lat.ix,n.lon,seasonal.fac,flag,
                                  input.list)
      print('Seasonal Averages Done')
    }
    ##----------------------------------------------------------
    ##Monthly Averages 
    if (type=='monthly') {
      monthly_averages_for_model(varname,mon.ncs,lat.ix,n.lon,monthly.fac,flag,
                                 input.list)
      print('Monthly Averages Time')
    }
    ##----------------------------------------------------------
    ##Annual Extremes
    if (type=='annual_extremes') {
      annual_extremes_for_model(varname,ext.ncs,lat.ix,n.lon,yearly.fac,flag,
                                input.list)
      print('Annual Extremes Time')
    }
    ##----------------------------------------------------------
    if (type=='annual_quantiles') {
    ##Annual Quantiles
      annual_quantiles_for_model(varname,pctl,quant.ncs,lat.ix,n.lon,yearly.fac,flag,
                                 input.list)
      print('Annual Quantiles Time')
    }
    ##----------------------------------------------------------

    rm(input.list)
  }##Longitude Loop
  nc_close(input.nc)

  print('Removing lat band files')
  file.remove(paste0(tmp.dir,"/",input.file))

}##Latitude File Loop

if (type=='annual') {
   nc_close(ann.ncs)
   file.copy(from=paste0(out.dir,ann.file),to=paste0(write.dir,'annual/'),overwrite=TRUE)
}

if (type=='seasonal') {
   nc_close(seas.ncs)
   file.copy(from=paste0(out.dir,seas.file),to=paste0(write.dir,'seasonal/'),overwrite=TRUE)
}

if (type=='monthly') {
   nc_close(mon.ncs)
   file.copy(from=paste0(out.dir,mon.file),to=paste0(write.dir,'monthly/'),overwrite=TRUE)
}

if (type=='annual_extremes') {
   nc_close(ext.ncs)
   file.copy(from=paste0(out.dir,ext.file),to=paste0(write.dir,'annual_extremes/'),overwrite=TRUE)
}

if (type=='annual_quantiles') {
   nc_close(quant.ncs)
   file.copy(from=paste0(out.dir,quant.file),to=paste0(write.dir,'annual_quantiles/'),overwrite=TRUE)
}


file.remove(obs.tmp)
file.remove(gcm.tmp)

