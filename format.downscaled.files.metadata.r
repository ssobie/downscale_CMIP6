##Script to create spatially complete DQM files that are 5 years in length

ptm <- proc.time()

library(ncdf4)
library(PCICt)

source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R')

##-----------------------------------------------------------------------------------------


##Get from the DQM file
get_time_atts <- function(cal,units) { 
  time.atts <- list(standard_name = "time",
                    units = units,
                    calendar = cal)
  return(time.atts)
}

get_date_bounds <- function(nc) {
  dates <- netcdf.calendar(nc)
  yst <-  gsub('-','',format(head(dates,1),'%Y-%m-%d'))
  yen <-  gsub('-','',format(tail(dates,1),'%Y-%m-%d'))
  rv <- c(yst,yen)
  return(rv)
}

get_standard_atts <- function(var.name) {
  lon.atts <- list(standard_name="longitude",
                   long_name = "longitude",
                   units = "degrees_east",
                   axis = "X")
  
  lat.atts <- list(standard_name="latitude",
                   long_name = "latitude",
                   units = "degrees_north",
                   axis = "Y")
  
  pr.atts <- list(standard_name = "precipitation_flux",
                  long_name = "Precipitation",
                  missing_value = 32767,
                  cell_methods = "time: mean",
                  units = "kg m-2 d-1")

  tasmax.atts <- list(standard_name = "air_temperature",
                      long_name = "Daily Maximum Near-Surface Air Temperature",
                      units = "degC",
                      missing_value = 32767,
                      cell_methods = "time: maximum")

  tasmin.atts <- list(standard_name = "air_temperature",
                      long_name = "Daily Minimum Near-Surface Air Temperature",
                      units = "degC",
                      missing_value = 32767,
                      cell_methods = "time: minimum")

  var.atts <- switch(var.name,
                     pr=pr.atts,
                     tasmax=tasmax.atts,
                     tasmin=tasmin.atts)

  rv <- list(lon=lon.atts,
             lat=lat.atts,
             var=var.atts)
  return(rv)
}

##Global Attributes
##gcm is the gcm name e.g. ACCESS1-0
##drive.institute is the centre e.g. CSIRO-BOM
##ssp is the SSP as SSP5 8.5
get_global_atts <- function(gcm.nc,ssp,run) {

  gcm.glob.atts <- ncatt_get(gcm.nc,0)

  global.atts <- list(institution="Pacific Climate Impacts Consortium (PCIC), Victoria, BC, www.pacificclimate.org",
                   contact="Pacific Climate Impacts Consortium",
                   Conventions="CF-1.7 CMIP-6.2",
                   institute_id ="PCIC",
                   domain='Canada',
                   creation_date=format(Sys.time(),'%Y-%m-%dT%H:%M:%S%Z'),
                   frequency="day",
                   product="downscaled-output",
                   mip_era = "CMIP6",
                   modeling_realm="atmos",
                   activity_id=gcm.glob.atts$activity_id,
                   table_id='day',
                   variable_id=gcm.glob.atts$variable_id,    
                   references="Alex J. Cannon, Stephen R. Sobie, and Trevor Q. Murdock, 2015: Bias Correction of GCM Precipitation by Quantile Mapping: How Well Do Methods Preserve Changes in Quantiles and Extremes?. J. Climate, 28, 6938–6959.",
                   downscaling_method="Quantile Delta Mapping",
                   downscaling_method_id='BCCAQv2',
                   downscaling_package_id='github.com/pacificclimate/ClimDown',
                   driving_branch_method=gcm.glob.atts$branch_method,
                   driving_data_specs_version=gcm.glob.atts$data_specs_version,
                   driving_experiment=paste("historical,",ssp,sep=''),
                   driving_experiment_id=paste("historical,",ssp,sep=''),
                   driving_forcing_index=gcm.glob.atts$forcing_index,
                   driving_grid=gcm.glob.atts$grid,
                   driving_grid_label=gcm.glob.atts$grid_label,
                   driving_further_url_info=gcm.glob.atts$further_info_url,
                   driving_institution = gcm.glob.atts$institution, ##Full name
                   driving_institute_id = gcm.glob.atts$institution_id, ##Acronym
                   driving_model_id = gcm.glob.atts$source_id,
                   driving_nominal_resolution=gcm.glob.atts$nominal_resolution,
                   driving_realization_index = substr(run,2,2), ##These integers are from the 'r1i1p1' code
                   driving_initialization_index=substr(run,4,4),
                   driving_physics_index = substr(run,6,6),
                   driving_forcing_index = substr(run,8,8),
                   driving_source_id = gcm.glob.atts$source_id,                  
                   driving_source_type = gcm.glob.atts$source_type,                  
                   target_institution = "Canadian Forest Service, Natural Resources Canada",
                   target_institute_id = "CFS-NRCan",
                   target_dataset = "ANUSPLIN interpolated Canada daily 300 arc second climate grids",
                   target_dataset_id = "ANUSPLIN300",
                   target_references = "McKenney, D.W., Hutchinson, M.F., Papadopol, P., Lawrence, K., Pedlar, J.,\nCampbell, K., Milewska, E., Hopkinson, R., Price, D., and Owen, T.,\n2011. Customized spatial climate models for North America.\nBulletin of the American Meteorological Society, 92(12): 1611-1622.\n\nHopkinson, R.F., McKenney, D.W., Milewska, E.J., Hutchinson, M.F.,\nPapadopol, P., Vincent, L.A., 2011. Impact of aligning climatological day\non gridding daily maximum-minimum temperature and precipitation over Canada.\nJournal of Applied Meteorology and Climatology 50: 1654-1665.",
                   target_version = "obtained: 2 April 2012, 14 June 2012, and 30 January 2013",
                   target_contact = "Pia Papadopol (pia.papadopol@nrcan-rncan.gc.ca)",
                   title = "Bias Correction/Constructed Analogue Quantile Mapping version 2.0 (BCCAQ2) downscaling model output for Canada")
  return(global.atts)
}

##Filename format is: 'pr_day_BCCAQ2+ANUSPLIN300+ACCESS1-0_historical+rcp45_r1i1p1_19500101-21001231.nc'

add_attributes_ncdf <- function(var.name, ds.nc, gcm.nc,global.atts) {

  atts <- get_standard_atts(var.name)
  print('Lon names')
  lon.names <- names(atts$lon)
  for (j in 1:length(atts$lon))
    ncatt_put(ds.nc,varid='lon',attname=lon.names[j],attval=atts$lon[[j]])
  print('Lat names')
  lat.names <- names(atts$lat)
  for (j in 1:length(atts$lat))
    ncatt_put(ds.nc,varid='lat',attname=lat.names[j],attval=atts$lat[[j]])
  print('Var names')
  var.names <- names(atts$var)
  for (j in 1:length(atts$var))
    ncatt_put(ds.nc,varid=var.name,attname=var.names[j],attval=atts$var[[j]])
  print('Time atts')
  ##Time attributes
  ncatt_put(ds.nc,varid='time',attname='units',attval=gcm.nc$dim$time$units)
  ncatt_put(ds.nc,varid='time',attname='long_name',attval='Time')
  ncatt_put(ds.nc,varid='time',attname='standard_name',attval='Time')
  ncatt_put(ds.nc,varid='time',attname='calendar',attval=gcm.nc$dim$time$calendar)
  print('Global atts')
  ##Global Attributes
  global.names <- names(global.atts)
  for (g in 1:length(global.atts))
    ncatt_put(ds.nc,varid=0,attname=global.names[g],attval=global.atts[[g]])

  ##Offset and scale attributes
  scaling <- get_scaling(var.name)
  ncatt_put(ds.nc,var.name,attname='add_offset',attval=scaling$offset,prec='double')
  ncatt_put(ds.nc,var.name,attname='scale_factor',attval=scaling$scale,prec='double')

  ##Clear extraneous history
  ncatt_put(ds.nc,varid=0,attname='history',attval='')
}

##**************************************************

add_the_metadata <- function(var.name,ssp,run,gcm.nc,ds.nc) {

  global.atts <- get_global_atts(gcm.nc,ssp,run)

  add_attributes_ncdf(var.name,ds.nc,gcm.nc,global.atts)

  print('Elapsed time')
  print(proc.time() - ptm)

}

