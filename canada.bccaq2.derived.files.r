##Script to generate empty derived files for Canada to receive the 
##derived variables calculated from the split CMIP6 BCCAQv2 files.

source('/storage/home/ssobie/code/repos/downscale_CMIP6/format.downscaled.files.metadata.r')

##-------------------------------------------------------------------------------------------


time_component <- function(time.file,freq) {

  nc <- nc_open(time.file,write=FALSE)

  time.atts <- ncatt_get(nc,'time')
  time.calendar <- time.atts$calendar
  time.units <- time.atts$units

  time.start <- as.Date(strsplit(time.units, ' ')[[1]][3])
  past.origin <- as.PCICt(strsplit(time.units, ' ')[[1]][3],
                          cal=time.calendar)
 
  past.values <- ncvar_get(nc,'time')
  full.series <- format(past.origin + past.values*86400,'%Y-%m-%d')
  full.years <- format(past.origin + past.values*86400,'%Y')
  print(range(full.years))
  years.ix <- grep('*-01-01',full.series)
  years <- past.values[years.ix]
  seas.ix <- sort(c(grep('*-01-15',full.series),
		    grep('*-04-15',full.series),
		    grep('*-07-15',full.series),
		    grep('*-10-15',full.series)))
  seasons <- past.values[seas.ix]		    
  months.ix <- grep('[0-9]{4}-[0-9]{2}-01',full.series)
  months <- past.values[months.ix]

  dates <- switch(freq,
                  Ann=years,
                  Mon=months,
		  Seas=seasons,
                  AnnClim=1,
                  SeasClim=1:4,
                  MonClim=1:12,
		  RP=1:3)

  new.int <- paste(range(full.years),collapse='-')
  print(new.int)
  dates <- as.numeric(dates)
  nc_close(nc)

  rv <- list(atts=time.atts,
             dates=dates,
	     interval=new.int)
  return(rv)
      
}

space_component <- function(space.file) {

  print(space.file)
  nc <- nc_open(space.file,write=FALSE)
  ##Attributes to retain
  lon <- ncvar_get(nc,'lon')
  lat <- ncvar_get(nc,'lat')  

  lon.atts <- ncatt_get(nc,'lon')
  lat.atts <- ncatt_get(nc,'lat')

  nc_close(nc)
  rv <- list(lon.atts=lon.atts,
             lat.atts=lat.atts,
             lon=lon,
             lat=lat)  
  return(rv)   
}

get_climdex_info <- function(climdex.name) {

  climdex.names <- list(climdex.fd=c('fdETCCDI','Ann','days'),
                        climdex.su=c('suETCCDI','Ann','days'),
                        climdex.su30=c('su30ETCCDI','Ann','days'),
                        climdex.id=c('idETCCDI','Ann','days'),
                        climdex.tr=c('trETCCDI','Ann','days'),
                        climdex.gsl=c('gslETCCDI','Ann','days'),
                        climdex.txx=c('txxETCCDI','Mon','degC'),
                        climdex.tnx=c('tnxETCCDI','Mon','degC'),
                        climdex.txn=c('txnETCCDI','Mon','degC'),
                        climdex.tnn=c('tnnETCCDI','Mon','degC'),
                        climdex.tn10p=c('tn10pETCCDI','Mon','days'),
                        climdex.tx10p=c('tx10pETCCDI','Mon','days'),
                        climdex.tn90p=c('tn90pETCCDI','Mon','days'),
                        climdex.tx90p=c('tx90pETCCDI','Mon','days'),
                        climdex.wsdi=c('wsdiETCCDI','Ann','days'),
                        climdex.csdi=c('csdiETCCDI','Ann','days'),
                        climdex.dtr=c('dtrETCCDI','Mon','degC'),
                        climdex.rx1day=c('rx1dayETCCDI','Mon','mm'),
                        climdex.rx2day=c('rx2dayETCCDI','Mon','mm'),
                        climdex.rx5day=c('rx5dayETCCDI','Mon','mm'),
                        climdex.sdii=c('sdiiETCCDI','Ann','mm d-1'),
                        climdex.r10mm=c('r10mmETCCDI','Ann','days'),
                        climdex.r20mm=c('r20mmETCCDI','Ann','days'),
                        climdex.cdd=c('cddETCCDI','Ann','days'),
                        climdex.cwd=c('cwdETCCDI','Ann','days'),
                        climdex.r95p=c('r95pETCCDI','Ann','mm'),
                        climdex.r99p=c('r99pETCCDI','Ann','mm'),
                        climdex.prcptot=c('prcptotETCCDI','Ann','mm'),
                        climdex.r95days=c('r95daysETCCDI','Ann','days'),
                        climdex.r99days=c('r99daysETCCDI','Ann','days'),
                        climdex.r95store=c('r95storeETCCDI','Ann','days'),
                        climdex.r99store=c('r99storeETCCDI','Ann','days'))

  rv <- climdex.names[[climdex.name]]
  return(rv)
}


##-----------------------------------------------------------

create_base_files <- function(var.name,var.units,long.name,
			      gcm,scenario,run,gcm.nc,
                              write.clim.name,
                              time.info,space.info,
                              data.dir,write.dir) {


  ##--------------------------------------------------------------
  ##Create new netcdf file
  x.geog <- ncdim_def('lon', 'degrees_east', space.info$lon)
  y.geog <- ncdim_def('lat', 'degrees_north', space.info$lat)
  t.geog <- ncdim_def('time', time.info$atts$units, time.info$dates,
                      unlim=FALSE, calendar=time.info$atts$calendar)
  
  var.geog <- ncvar_def(var.name, units=var.units, dim=list(x.geog, y.geog, t.geog),
                        missval=-32768.)
  print('Create this file')			
  print(paste(write.dir,write.clim.name,sep=''))
  file.nc <- nc_create(paste(write.dir,write.clim.name,sep=''), var.geog)
  
  ##Loop over subsets of the time series
  ##Past file first
  global.atts <- get_global_atts(gcm.nc,scenario,run)

  global.names <- names(global.atts)
  for (g in 1:length(global.names)) {
    ncatt_put(file.nc,varid=0,attname=global.names[g],attval=global.atts[[g]])
  }
  print('Finished Global Atts')
  
  ##Time attributes
  ncatt_put(file.nc,varid='time',attname='units',attval=time.info$atts$units)
  ncatt_put(file.nc,varid='time',attname='long_name',attval='Time')
  ncatt_put(file.nc,varid='time',attname='standard_name',attval='Time')
  ncatt_put(file.nc,varid='time',attname='calendar',attval=time.info$atts$calendar)  
  print('Finished time atts')
  
  lon.atts <- space.info$lon.atts
  lon.names <- names(lon.atts)
  for (j in 1:length(lon.atts)) {
    ncatt_put(file.nc,varid='lon',attname=lon.names[j],attval=lon.atts[[j]])
  }
  ncvar_put(file.nc,varid='lon',vals=space.info$lon)
  print('Finished Lon atts')
  
  lat.atts <- space.info$lat.atts
  lat.names <- names(lat.atts)
  for (j in 1:length(lat.atts)) {
    ncatt_put(file.nc,varid='lat',attname=lat.names[j],attval=lat.atts[[j]])
  }
  ncvar_put(file.nc,varid='lat',vals=space.info$lat)
  print('Finished lat atts')
  
  ##Climdex Attributes
  ncatt_put(file.nc,varid=var.name,attname='units',attval=var.units)
  ncatt_put(file.nc,varid=var.name,attname='_FillValue',attval=-32768.)
  ncatt_put(file.nc,varid=var.name,attname='standard_name',attval=long.name)
  ncatt_put(file.nc,varid=var.name,attname='long_name',attval=long.name)
  print('Finished Var atts')
  nc_close(file.nc)
}



##**************************************************************************************



##------------------------------------------------------------------------

make_degree_day_files <- function(degree.names,gcm,scenario,run,
                                  tasmax.tmp,obs.tmp,gcm.tmp,
                                  data.dir,write.dir,tmp.dir) {
  
  ##degree.names <- c('cdd','fdd','gdd','hdd')

  freq <- 'Ann'

  out.dir <- paste(tmp.dir,'degree_days/',sep='')
  if (!file.exists(out.dir)) {
    dir.create(out.dir,recursive=TRUE)
  }
  time.info <- time_component(tasmax.tmp,freq)
  space.info <- space_component(obs.tmp)  
  dd.files <- rep('A',length(degree.names))
  gcm.nc <- nc_open(gcm.tmp)
  for (d in seq_along(degree.names)) {
      degree.name <- degree.names[d]
      long.name <- switch(degree.name,
                          cdd='Cooling Degree Days',
                          fdd='Freezing Degree Days',
  		     	  gdd='Growing Degree Days',
                          hdd='Heating Degree Days')

      dd.files[d] <- write.dd.name <- paste0(degree.name,'_annual_BCCAQ2v2+ANUSPLIN300_',gcm,'_historical+',scenario,'_',run,'_',time.info$interval,'.nc')    
      create_base_files(degree.name,'degree_days',long.name,
                        gcm,scenario,run,gcm.nc,
                        write.dd.name,
                        time.info,space.info,		
                        tmp.dir,out.dir)
  }
  nc_close(gcm.nc)

  return(dd.files)
}

##-----------------------------------------------------------------------
##Return Periods

make_return_period_file <- function(var.name,gcm,scenario,run,
                                     ds.tmp,obs.tmp,gcm.tmp,
                                     data.dir,out.dir,tmp.dir) {

  ##Creating Annual Maximum Values here so any return period year can be calculated
  print('Creating Return Period Annual Maximum base files')
  long.name <- switch(var.name,
                      pr='Annual Maximum Precipitation',
                      tasmax='Annual Maximum Tasmax',
		      tasmin='Annual Minimum Tasmin')
  freq <- 'Ann'                      

  if (!file.exists(out.dir)) {
    dir.create(out.dir,recursive=TRUE)
  }

  time.info <- time_component(ds.tmp,freq)
  space.info <- space_component(obs.tmp)  
  gcm.nc <- nc_open(gcm.tmp)
  var.units <- switch(var.name,
                       tasmax='degC',
                       tasmin='degC',
                       pr='mm day-1')
  write.rp.name <- paste0(var.name,'_annual_maximum_BCCAQ2v2+ANUSPLIN300_',gcm,
                          '_historical+',scenario,'_',run,'_',time.info$interval,'.nc')    
  if (var.name=='tasmin') {
      write.rp.name <- paste0(var.name,'_annual_minimum_BCCAQ2v2+ANUSPLIN300_',gcm,
                       '_historical+',scenario,'_',run,'_',time.info$interval,'.nc')    
  }

  create_base_files(var.name,var.units,long.name,
                    gcm,scenario,run,gcm.nc,    
                    write.rp.name,
                    time.info,space.info,		
                    tmp.dir,out.dir)
  nc_close(gcm.nc)
  rv <- write.rp.name
  return(rv)
}

##------------------------------------------------------------------------
##Quantiles

make_quantile_file <- function(var.name,gcm,scenario,run,pctl,
                                ds.tmp,obs.tmp,gcm.tmp,
                                data.dir,out.dir,tmp.dir) {

  ##Quantiles
  ##Creating Annual Quantile values for the building code parameters
  print('Creating Return Period Annual Maximum base files')
  long.name <- switch(var.name, 
                     pr='Annual Precipitation Quantile',
  		     tasmax='Annual Tasmax Quantile',
		     tasmin='Annual Tasmin Quantile')
  freq <- 'Ann'                      

  if (!file.exists(out.dir)) {
    dir.create(out.dir,recursive=TRUE)
  }

  time.info <- time_component(ds.tmp,freq)
  space.info <- space_component(obs.tmp)  

  gcm.nc <- nc_open(gcm.tmp)
  var.units <- switch(var.name,
                      tasmax='degC',
                      tasmin='degC',
                      pr='mm day-1')

  write.qts.name <- paste0(var.name,'_annual_quantile_',pctl,'_BCCAQ2v2+ANUSPLIN300_',gcm,
                     '_historical+',scenario,'_',run,'_',time.info$interval,'.nc')    

  create_base_files(var.name,var.units,paste0(long.name,' ',pctl),
      		      gcm,scenario,run,gcm.nc,    
                      write.qts.name,
                      time.info,space.info,		
                      tmp.dir,out.dir)

  nc_close(gcm.nc)
  rv <- write.qts.name
  return(rv)

}


##-----------------------------------------------------------------------
##Annual Averages/Sums

make_annual_file <- function(var.name,gcm,scenario,run,
                               ds.tmp,obs.tmp,gcm.tmp,
                               data.dir,out.dir,tmp.dir) {
  gcm.nc <- nc_open(gcm.tmp) 
  var.units <- switch(var.name,
                      tasmax='degC',
                      tasmin='degC',
                      pr='mm day-1')
  var.type <- switch(var.name,
                      tasmax='average',
                      tasmin='average',
                      pr='total')
  long.name <- switch(var.name,
                      pr='Total Precipitation',
                      tasmax='Average Maximum Temperature',
  		      tasmin='Average Minimum Temperature')

  ##---------------------------
  ##Annual
  freq <- 'Ann'
  time.info <- time_component(ds.tmp,freq)
  space.info <- space_component(obs.tmp)  

  if (!file.exists(out.dir)) {
    dir.create(out.dir,recursive=TRUE)
  }      
  write.ann.name <- paste0(var.name,'_annual_',var.type,'_BCCAQ2v2+ANUSPLIN300_',gcm,
                           '_historical+',scenario,'_',run,'_',time.info$interval,'.nc')    
  create_base_files(var.name,var.units,paste0('Annual ',long.name),
                    gcm,scenario,run,gcm.nc,
                    write.ann.name,
                    time.info,space.info,		
                    tmp.dir,out.dir)
  nc_close(gcm.nc)
  rv <- write.ann.name
  return(rv)  

}

##-----------------------------------------------------------------------
##Seasonal Averages/Sums

make_seasonal_file <- function(var.name,gcm,scenario,run,
                               ds.tmp,obs.tmp,gcm.tmp,
                               data.dir,out.dir,tmp.dir) {
  gcm.nc <- nc_open(gcm.tmp) 
  var.units <- switch(var.name,
                      tasmax='degC',
                      tasmin='degC',
                      pr='mm day-1')
  var.type <- switch(var.name,
                      tasmax='average',
                      tasmin='average',
                      pr='total')
  long.name <- switch(var.name,
                      pr='Total Precipitation',
                      tasmax='Average Maximum Temperature',
  		      tasmin='Average Minimum Temperature')

  ##---------------------------
  ##Seasonal
  freq <- 'Seas'
  time.info <- time_component(ds.tmp,freq)
  space.info <- space_component(obs.tmp)  

  if (!file.exists(out.dir)) {
    dir.create(out.dir,recursive=TRUE)
  }      
  write.seas.name <- paste0(var.name,'_seasonal_',var.type,'_BCCAQ2v2+ANUSPLIN300_',gcm,
                           '_historical+',scenario,'_',run,'_',time.info$interval,'.nc')    
  create_base_files(var.name,var.units,paste0('Seasonal ',long.name),
                    gcm,scenario,run,gcm.nc,
                    write.seas.name,
                    time.info,space.info,		
                    tmp.dir,out.dir)
  nc_close(gcm.nc)
  rv <- write.seas.name
  return(rv)  

}

##-----------------------------------------------------------------------
##Monthly Averages/Sums

make_monthly_file <- function(var.name,gcm,scenario,run,
                              ds.tmp,obs.tmp,gcm.tmp,
                              data.dir,out.dir,tmp.dir) {
  gcm.nc <- nc_open(gcm.tmp) 
  var.units <- switch(var.name,
                      tasmax='degC',
                      tasmin='degC',
                      pr='mm day-1')
  var.type <- switch(var.name,
                      tasmax='average',
                      tasmin='average',
                      pr='total')
  long.name <- switch(var.name,
                      pr='Total Precipitation',
                      tasmax='Average Maximum Temperature',
  		      tasmin='Average Minimum Temperature')
  ##---------------------------
  ##Monthly
  freq <- 'Mon'
  time.info <- time_component(ds.tmp,freq)
  space.info <- space_component(obs.tmp)  

  if (!file.exists(out.dir)) {
    dir.create(out.dir,recursive=TRUE)
  }      
  write.mon.name <- paste0(var.name,'_monthly_',var.type,'_BCCAQ2v2+ANUSPLIN300_',gcm,
                           '_historical+',scenario,'_',run,'_',time.info$interval,'.nc')    
  create_base_files(var.name,var.units,paste0('Monthly ',long.name),
                    gcm,scenario,run,gcm.nc,
                    write.mon.name,
                    time.info,space.info,		
                    tmp.dir,out.dir)
  nc_close(gcm.nc)
  rv <- write.mon.name
  return(rv)  

}

##-----------------------------------------------------------------------
##CLIMDEX

make_climdex_file <- function(var.name,gcm,scenario,run,
                              ds.tmp,obs.tmp,gcm.tmp,
                              data.dir,write.dir,tmp.dir) {

  gcm.nc <- nc_open(gcm.tmp) 
  if (!file.exists(out.dir)) {
    dir.create(out.dir,recursive=TRUE)
  }
  if (!file.exists(paste0(write.dir,'climdex/'))) {
     dir.create(paste0(write.dir,'climdex/'),recursive=TRUE)
  }   

  print(var.name)
  climdex.info <- get_climdex_info(paste0('climdex.',var.name))
  climdex.var <- climdex.info[1]
  climdex.calendar <- climdex.info[2]
  climdex.units <- climdex.info[3]

  time.info <- time_component(ds.tmp,freq=climdex.calendar)
  space.info <- space_component(obs.tmp)  

  var.units <- climdex.units
  write.clim.name <- paste0(climdex.var,'_',tolower(climdex.calendar),'_BCCAQ2v2+ANUSPLIN300_',gcm,
                    '_historical+',scenario,'_',run,'_',time.info$interval,'.nc')    

  create_base_files(climdex.var,var.units,toupper(climdex.var),
                    gcm,scenario,run,gcm.nc,
                    write.clim.name,
                    time.info,space.info,		
                    tmp.dir,out.dir)
  nc_close(gcm.nc)
  return(write.clim.name)
}


