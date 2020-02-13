##Script to calculate the ARI (antecedent rainfall index)

ptm <- proc.time()

library(PCICt)
library(ncdf4)

source('/storage/home/ssobie/code/repos/downscale_CMIP6/format.downscaled.files.metadata.r')

##------------------------------------------------------------
##Pull the time series
time_series_from_nc <- function(nc) {

  time.atts <- ncatt_get(nc,'time')
  time.calendar <- time.atts$calendar
  time.units <- time.atts$units

  time.start <- as.Date(strsplit(time.units, ' ')[[1]][3])
  origin <- as.PCICt(strsplit(time.units, ' ')[[1]][3],
                     cal=time.calendar)
  time.values <- ncvar_get(nc,'time')
  time.series <- origin + time.values*86400

  time.min <- as.character(format(head(time.series,1),'%Y%m%d'))
  time.max <- as.character(format(tail(time.series,1),'%Y%m%d'))

  rv <- list(units=time.units,
             values=time.values,
             series=time.series,
             calendar=time.calendar,
             tmax=time.max,
             tmin=time.min)
  return(rv)
}

##------------------------------------------------------------
##Split files for EC into 5-year pieces

time_split_of_bccaqv2_files <- function(bccaq2.file,tmp.dir,ec.dir) {

   years <- c(1950,seq(1956,2096,5))                             
   cnts <- c(5,rep(4,length(years)-1))
   for (i in seq_along(years)) {
      yst <- years[i]
      yen <- years[i] + cnts[i]
      tmp.file <- paste0('TIME_SUB_',yst,'-',yen,'.nc')
      work <- paste('cdo seldate,',yst,'-01-01T00:00:00,',yen,'-12-31T23:59:59 ',tmp.dir,bccaq2.file,' ',tmp.dir,tmp.file,sep='')
      print(work)
      system(work)

      tnc <- nc_open(paste0(tmp.dir,tmp.file),write=TRUE)
      tmp.time <- time_series_from_nc(tnc) 
      ncatt_put(tnc,varid=0,attname='history',attval='')
      nc_close(tnc)

      sub.time.file <- gsub(pattern='[0-9]{8}-[0-9]{8}',
                       replacement=paste0(tmp.time$tmin,'-',tmp.time$tmax),bccaq2.file)

      file.rename(from=paste0(tmp.dir,tmp.file),to=paste0(tmp.dir,sub.time.file))
      file.copy(from=paste0(tmp.dir,sub.time.file),to=ec.dir,overwrite=TRUE)
      file.remove(from=paste0(tmp.dir,sub.time.file))
   }
}


##------------------------------------------------------------

get_var_units <- function(var.name) {
   rv <- switch(var.name,
                pr='kg m-2 d-1',
                tasmax='degC',
                tasmin='degC')
   return(rv)
}


##------------------------------------------------------------

make_bccaqv2_file <- function(var.name,gcm,run,scenario,
                              gcm.file,obs.file,                                   
                              tmp.dir) {

  obs.nc <- nc_open(paste0(tmp.dir,obs.file))
  lon <- ncvar_get(obs.nc,'lon')
  nlon <- length(lon)
  lat <- ncvar_get(obs.nc,'lat')
  nlat <- length(lat)
  nc_close(obs.nc)

  gcm.nc <- nc_open(paste0(tmp.dir,gcm.file))
  grid <- ncatt_get(gcm.nc,0)$grid_label
  ##Consider acquiring the other labels (gcm,scenario) similarly?
  time <- time_series_from_nc(gcm.nc)
  
  bccaqv2.file <- paste0(varname,'_day_BCCAQv2+ANUSPLIN300_',gcm,'_historical+',scenario,'_',run,'_',grid,'_',
                         time$tmin,'-',time$tmax,'.nc')

  x.geog <- ncdim_def('lon', 'degrees_east', lon)
  y.geog <- ncdim_def('lat', 'degrees_north', lat) 
  t.geog <- ncdim_def('time', time$units, time$values,
                       unlim=FALSE, calendar=time$calendar)

  var.geog <- ncvar_def(var.name, units=get_var_units(var.name),
                        dim=list(x.geog, y.geog, t.geog),
                        missval=32767,prec='short')

  print('Creating netcdf')
  write.nc <- nc_create(paste0(tmp.dir,bccaqv2.file), var.geog,h_minfree=104857)

  print('Adding metadata')
  add_the_metadata(var.name,scenario,run,gcm.nc,write.nc)

  print('Adding Lon and Lat')
  ncvar_put(write.nc,'lon',lon)
  ncvar_put(write.nc,'lat',lat)
  nc_close(gcm.nc)

  rv <- list(file=bccaqv2.file,nc=write.nc)
  return(rv)
  
}

##------------------------------------------------------------
##Function From James

compute_scale_and_offset <- function(minimum, maximum, n) {
    # stretch/compress data to the available packed range
    scale.factor <- (maximum - minimum) / (2 ** n - 1)
    # translate the range to be symmetric about zero
    add.offset <- minimum + 2 ** (n - 1) * scale.factor
    c(scale.factor, add.offset)
}

##------------------------------------------------------------

get_scaling <- function(var.name) {
  ##Offset and scale for 16-bit integer
  if (grepl('tas',var.name)) { ##-100 to 100 degrees
    scale_factor <- 0.003051804
    add_offset <- 0.001525902
  }
  if (var.name=='pr') { ##0 to 1000 mm
    scale_factor <- 0.01525902
    add_offset <- 500.00762951
  }
  rv <- list(scale=scale_factor,
             offset=add_offset)
  return(rv)
}


##------------------------------------------------------------

pack_data_for_insertion <- function(input,var.name) {

  if (var.name == 'pr') {
     input[input >= 1000] <- 990
  }

  scaling <- get_scaling(var.name)
  rv <- round((input - scaling$offset) / scaling$scale)
  rm(input)
  return(rv)

}

##------------------------------------------------------------

insert_split_file_data <- function(var.name,write.nc,tmp.dir,split.dir) {

   splits <- rep(1:34,each=15)
   
   for (i in 1:34) {
      ix <- which(splits %in% i)
      lat.ix <- which(splits %in% i)
      lat.st <- lat.ix[1]
      lat.en <- tail(lat.ix,1)
      lat.cnt <- diff(range(lat.ix))+1

      ix.st <- ix[1]
      ix.cnt <- diff(range(ix))+1
     
      split.file <- list.files(path=split.dir, pattern=paste0(var.name,'_L',sprintf('%02d',i),'_',sprintf('%02d',lat.st)))
      print(split.file)
      
      ###file.copy(from=paste0(split.dir,'/',split.file),to=tmp.dir,overwrite=TRUE)
      Sys.sleep(5)

      split.nc <- nc_open(paste0(split.dir,split.file))
      ntime <- length(ncvar_get(split.nc,'time'))
      nlon <- length(ncvar_get(split.nc,'lon'))
      nlat <- length(ncvar_get(split.nc,'lat'))
      split.insert <- array(NA,c(nlon,nlat,ntime))
      for (j in 1:nlat) {      
         split.sub <- ncvar_get(split.nc,var.name,start=c(1,j,1),count=c(-1,1,-1))
         split.sub[is.nan(split.sub)] <- NA
         ##Pack data prior to insertion
         split.insert[,j,] <- pack_data_for_insertion(split.sub,var.name)
         rm(split.sub)
      }
      ncvar_put(write.nc,var.name,split.insert,start=c(1,ix.st,1),count=c(nlon,ix.cnt,ntime))
      rm(split.insert)
      nc_close(split.nc)
      file.remove(paste0(split.dir,split.file))
      print('Copied back and cleaned up') 
      gc()
   }
}

##------------------------------------------------------------

##************************************************************

args <- commandArgs(trailingOnly=TRUE)
for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
}

##varname <- 'tasmax'
##gcm <- 'CanESM5'
##run <- 'r2i1p2f1'
##scenario <- 'ssp585'
##tmpdir <- '/local_temp/ssobie/ds_assembly/'

var.name <- varname
gcm.dir <- paste0('/storage/data/climate/CMIP6/assembled/',gcm,'/north_america/')
gcm.file <- paste0(varname,'_day_',gcm,'_North_America_historical+',scenario,'_',run,'_gn_19500101-21001231.nc')

raw.dir <- '/storage/data/climate/downscale/BCCAQ2/raw_downscaled/'
ds.dir <- paste0(raw.dir,'ds_',varname,'_',gcm,'_',run,'_',scenario,'_split15')
write.dir <- paste0('/storage/data/climate/downscale/BCCAQ2/CMIP6_BCCAQv2/',gcm,'/')
if (!file.exists(write.dir)) {
  dir.create(write.dir,recursive=T)
}

ec.dir <- paste0(write.dir,gcm,'_',varname,'_',run,'_',scenario,'_5_year_files/')
if (!file.exists(ec.dir)) {
  dir.create(ec.dir,recursive=T)
}


obs.dir <- '/storage/data/climate/observations/gridded/ANUSPLIN/ANUSPLIN_300ARCSEC/'
obs.file <- 'anusplin_template_one.nc'

tmp.dir <- paste0(tmpdir,'combine_',gcm,'_',varname,'_',run,'_',scenario,'_tmp/')
if (!file.exists(tmp.dir)) {
  dir.create(tmp.dir,recursive=T)
}
 
file.copy(from=paste0(obs.dir,obs.file),to=tmp.dir)
print('Done copying observations')
file.copy(from=paste0(gcm.dir,gcm.file),to=tmp.dir)
print('Done copying gcm file')
file.copy(from=ds.dir,to=tmp.dir,recursive=TRUE)
print('Done copying DS directory')
print('Done Copying')

split.dir <- paste0(tmp.dir,'ds_',varname,'_',gcm,'_',run,'_',scenario,'_split15/')

##-----------------------------------------------

print('Making File') 

bccaqv2.file <- make_bccaqv2_file(var.name=varname,gcm=gcm,run=run,scenario=scenario,
                                  gcm.file=gcm.file,obs.file=obs.file,                              
                                  tmp.dir=tmp.dir)
file.remove(paste0(tmp.dir,gcm.file))
file.remove(paste0(tmp.dir,obs.file))

print('Split file data insertion') 
insert_split_file_data(var.name,bccaqv2.file$nc,tmp.dir,split.dir)
nc_close(bccaqv2.file$nc)

print('Copying to write dir')
file.copy(from=paste0(tmp.dir,bccaqv2.file$file),to=write.dir,overwrite=TRUE)

##Split the file by years (5-Years?) to make the transfer 
##possible for EC
##-----------------------------------------------

##bccaqv2.file <- list(file="tasmax_day_BCCAQv2+ANUSPLIN300_CanESM5_historical+ssp585_r2i1p2f1_gn_19500101-21001231.nc")

time_split_of_bccaqv2_files(bccaqv2.file$file,tmp.dir,ec.dir)

##-----------------------------------------------


file.remove(paste0(tmp.dir,bccaqv2.file$file))
rm.files <- list.files(path=paste0(tmp.dir,gcm,'/'),recursive=T,full.name=T)
file.remove(rm.files)
print('Copied back and cleaned up')

print('Elapsed time')
print(proc.time()-ptm)

