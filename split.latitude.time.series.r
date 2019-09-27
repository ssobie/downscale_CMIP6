##Divide the BCCI file into latitude bands of 10 cells 

library(ncdf4)
library(PCICt)

ptm <- proc.time()

##----------------------------------------------------------------

time_series_from_nc <- function(nc) {

  time.atts <- ncatt_get(nc,'time')
  time.calendar <- time.atts$calendar
  time.units <- time.atts$units

  time.start <- as.Date(strsplit(time.units, ' ')[[1]][3])
  origin <- as.PCICt(strsplit(time.units, ' ')[[1]][3],
                     cal=time.calendar)
  time.values <- ncvar_get(nc,'time')
  time.series <- origin + time.values*86400

  rv <- list(units=time.units,
             values=time.values,
	     series=time.series,
             calendar=time.calendar)
  return(rv)
}

##-------------------------------------------------------------------

get_var_units <- function(var.name) {

  rv <- switch(var.name,
               pr='kg m-2 d-1',
	       tasmax='degC',
               tasmin='degC')
  return(rv)
}

##-----------------------------------------------------------------------

make_subset_file <- function(nc,var.name,lat.ix,split.dir,split.file) {

  bcci.time <- time_series_from_nc(nc)
  
  lon <- ncvar_get(nc,'lon')
  lat.sub <- ncvar_get(nc,'lat')[lat.ix]

  x.geog <- ncdim_def('lon', 'degrees_east', lon)
  y.geog <- ncdim_def('lat', 'degrees_north', lat.sub)
  t.geog <- ncdim_def('time', bcci.time$units, bcci.time$values,
                       unlim=FALSE, calendar=bcci.time$calendar)

  var.geog <- ncvar_def(var.name, units=get_var_units(var.name), dim=list(x.geog, y.geog, t.geog),
                        missval=-32768)
  print('Making this file:')
  print(paste0(split.dir,split.file))
  file.nc <- nc_create(paste0(split.dir,split.file), var.geog) ##,h_minfree=104857)

  ncatt_put(file.nc,varid=var.name,attname='units',attval=get_var_units(var.name))
  ncatt_put(file.nc,varid=var.name,attname='_FillValue',attval=-32768)
  ncatt_put(file.nc,varid=var.name,attname='standard_name',attval=toupper(var.name))
  ncatt_put(file.nc,varid=var.name,attname='long_name',attval=toupper(var.name))

  ncatt_put(file.nc,varid='time',attname='units',attval=bcci.time$units)
  ncatt_put(file.nc,varid='time',attname='long_name',attval='Time')
  ncatt_put(file.nc,varid='time',attname='standard_name',attval='Time')
  ncatt_put(file.nc,varid='time',attname='calendar',attval=bcci.time$calendar)

  lon.atts <- ncatt_get(nc,'lon')
  lon.names <- names(lon.atts)
  for (j in 1:length(lon.atts))
    ncatt_put(file.nc,varid='lon',attname=lon.names[j],attval=lon.atts[[j]])

  lat.atts <- ncatt_get(nc,'lat')
  lat.names <- names(lat.atts)
  for (j in 1:length(lat.atts))
    ncatt_put(file.nc,varid='lat',attname=lat.names[j],attval=lat.atts[[j]])
  ncvar_put(file.nc,'lon',lon)
  ncvar_put(file.nc,'lat',lat.sub)

  nc_close(file.nc)
}


##------------------------------------------------------------------------------

split.into.lat.band <- function(bcci.nc,
		                var.name,
				lat.ix,lat.st,lat.en,lat.cnt,
				split.file,
				split.dir) {

  bcci.band <- ncvar_get(bcci.nc,var.name,start=c(1,lat.st,1),count=c(-1,lat.cnt,-1))
  make_subset_file(bcci.nc,var.name,lat.ix,split.dir,split.file)
  sub.nc <- nc_open(paste0(split.dir,split.file),write=TRUE)
  ncvar_put(sub.nc,var.name,bcci.band)
  nc_close(sub.nc)
}

##------------------------------------------------------------

split_apart_bcci_file <- function(var.name,gcm,bccifile,
                                  tmp.dir,write.dir) {

   bcci.nc <- nc_open(paste0(tmp.dir,bccifile))
   split.dir <- paste0(tmp.dir,gcm,'_split/')
   if (!file.exists(split.dir)) {
      dir.create(split.dir,recursive=T)
   }

   ##Divide up the BCCI latitude (510 total) into 10-cell bands:
   ###splits <- rep(1:51,each=10)
   ##Divide up the BCCI latitude (510 total) into 25-cell bands:
   splits <- rep(1:30,each=17)

   for (i in 1:17) {
      lat.ix <- which(splits %in% i)
      
      lat.st <- lat.ix[1]
      lat.en <- tail(lat.ix,1)
      lat.cnt <- diff(range(lat.ix))+1
  
      split.file <- gsub(var.name,
       	                 paste0(var.name,'_L',sprintf('%02d',i),'_',sprintf('%02d',lat.st),'-',sprintf('%02d',lat.en)),
	     	         bccifile)

      split.into.lat.band(bcci.nc,var.name,
                          lat.ix,lat.st,lat.en,lat.cnt,
                          split.file,split.dir)
   }
   print('Copying split files back from temp')
   file.copy(from=split.dir,to=write.dir,recursive=TRUE,overwrite=TRUE)
   nc_close(bcci.nc)    
   file.remove(paste0(tmp.dir,bccifile))
   files.rm <- list.files(path=split.dir,full.name=T)
   file.remove(files.rm)
}


##Testing
var.name <- 'tasmax'
gcm <- 'CanESM5'
bccifile <- 'tasmax_BCCI_day_CanESM5_North_America_historical+ssp585_r1i1p2f1_gn_19500101-21001231.nc'
##bccifile <- 'anusplin_tasmax_final.nc'
tmp.dir <- '/local_temp/ssobie/bcci_split/'
##tmp.dir <- '/local_temp/ssobie/obs_split/'
write.dir <- '/storage/data/climate/downscale/BCCAQ2/BCCI/'
##write.dir <- '/storage/data/climate/observations/gridded/ANUSPLIN/ANUSPLIN_300ARCSEC/anusplin_tasmax_split/'


split_apart_bcci_file(var.name,gcm,bccifile,
                      tmp.dir,write.dir)

 



print('Elapsed time')
print(proc.time()-ptm)
