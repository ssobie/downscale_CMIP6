##Script to calculate and write the standard set of derived variables
##for the 800m data

ptm <- proc.time()
cedar <- FALSE

if (cedar) {
   source('/home/ssobie/assessments/new.netcdf.calendar.R')
   source('/home/ssobie/assessments/cedar.derived.variable.support.r')
} else {
   source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R',chdir=T)
   source('/home/ssobie/code/repos/derived_gridded_indices/cedar.simplified.climdex.support.r')
}

library(ncdf4)
library(PCICt)
library(climdex.pcic)

library(foreach)

##----------------------------------------------------------------------------------------------
annual.averages.for.model <- function(ann.name,ann.ncs,lat.ix,n.lon,yearly.fac,flag,
                                      ann.list) {                                            
   ##Variables
       flen <- sum(!flag)
       ann.fx <- ann.fxns[[ann.name]]
       sub.list <- ann.list[!flag]
       rm(ann.list)
       
       ann.avg.values <- foreach(
                         data=sub.list,
                         .export=c('yearly.fac','ann.fx')
                         ) %do% {
                              ann.avg.values <- ann.fx(data,yearly.fac)
                         }
      rm(sub.list)                         
      ncol <- length(ann.avg.values[[1]])
      ann.avg.matrix <- matrix(NA,nrow=n.lon,ncol=ncol)
      sub.matrix <- matrix(unlist(ann.avg.values),nrow=flen,ncol=ncol,byrow=TRUE)
      rm(ann.avg.values)
      ann.avg.matrix[!flag] <- sub.matrix
      rm(sub.matrix)
      ncvar_put(ann.ncs,varid=paste0(ann.name,'ETCCDI'),vals=ann.avg.matrix,
                start=c(1,lat.ix,1),count=c(-1,1,-1))
      rm(ann.avg.matrix)                
      gc()
}

##----------------------------------------------------------------------------------------------
monthly.averages.for.model <- function(mon.name,mon.ncs,lat.ix,n.lon,monthly.fac,flag,
                                       mon.list) {
      
   ##Variables
   flen <- sum(!flag)
   mon.fx <- mon.fxns[[mon.name]]
   sub.list <- mon.list[!flag]
   rm(mon.list)

   mon.avg.values <- foreach(
                             data=sub.list,
                             .export=c('monthly.fac','mon.fx')
                             ) %do% {
                                 mon.avg.values <- mon.fx(data,monthly.fac)
                             }
   rm(sub.list)                         
			 
   ncol <- length(mon.avg.values[[1]])
   mon.avg.matrix <- matrix(NA,nrow=n.lon,ncol=ncol)      
   sub.matrix <- matrix(unlist(mon.avg.values),nrow=flen,ncol=ncol,byrow=TRUE)
   rm(mon.avg.values)
   mon.avg.matrix[!flag,] <- sub.matrix
   rm(sub.matrix)
   ncvar_put(mon.ncs,varid=paste0(mon.name,'ETCCDI'),vals=mon.avg.matrix,
            start=c(1,lat.ix,1),count=c(-1,1,-1))
   rm(mon.avg.matrix)
   gc()
}

##----------------------------------------------------------------------------------------------
r9.precip.for.model <- function(varname,ann.names,ann.ncs,lat.ix,n.lon,yearly.fac,dates,flag,
                                      ann.list) {
      
   ##Variables
   flen <- sum(!flag)
   sub.list <- ann.list[!flag]
   rm(ann.list)

   ann.fx <- ann.fxns[[varname]]

   ann.avg.values <- foreach(
                             data=sub.list,
                             .export=c('yearly.fac','ann.fx','dates')
                             ) %do% {
                                  ann.avg.values <- ann.fx(data,yearly.fac,dates)
                             }
   rm(sub.list)                         

   totals <- lapply(ann.avg.values,function(x){x$total})
   days <- lapply(ann.avg.values,function(x){x$days})
   stores <-  lapply(ann.avg.values,function(x){x$store}) 
   rm(ann.avg.values)
   ncol <- length(totals[[1]])

   totals.matrix <- matrix(NA,nrow=n.lon,ncol=ncol)
   sub.totals <- matrix(unlist(totals),nrow=flen,ncol=ncol,byrow=TRUE)
   rm(totals)
   totals.matrix[!flag,] <- sub.totals
   ncvar_put(ann.ncs[[1]],varid=paste0(ann.names[1],'ETCCDI'),vals=totals.matrix,
             start=c(1,lat.ix,1),count=c(-1,1,-1))
   rm(totals.matrix)

   days.matrix <- matrix(NA,nrow=n.lon,ncol=ncol)
   sub.days <- matrix(unlist(days),nrow=flen,ncol=ncol,byrow=TRUE)
   rm(days)
   days.matrix[!flag,] <- sub.days
   ncvar_put(ann.ncs[[2]],varid=paste0(ann.names[2],'ETCCDI'),vals=days.matrix,
             start=c(1,lat.ix,1),count=c(-1,1,-1))
   rm(days.matrix)

   store.matrix <- matrix(NA,nrow=n.lon,ncol=ncol)
   sub.store <- matrix(unlist(stores),nrow=flen,ncol=ncol,byrow=TRUE)
   rm(stores)
   store.matrix[!flag,] <- sub.store
   ncvar_put(ann.ncs[[3]],varid=paste0(ann.names[3],'ETCCDI'),vals=store.matrix,
             start=c(1,lat.ix,1),count=c(-1,1,-1))
   rm(store.matrix)

}

##--------------------------------------------------------------
##****************************************************************

if (1==0) {
  args <- commandArgs(trailingOnly=TRUE)
  for(i in 1:length(args)){
      eval(parse(text=args[[i]]))
  }
}

tmp.dir <- '/local_temp/ssobie/prism/' ##tmpdir

gcm <- 'ACCESS1-0'
scenario <- 'rcp85'
run <- 'r1i1p1'
interval <- '1950-2100'
type <- 'annual'
varname <- 'prcptot'

input.name <- input.varname[[varname]]
climdex.name <- paste0('climdex.',varname)
climdex.info <- get.climdex.info(climdex.name)

if (cedar) {
   base.dir <- '/scratch/ssobie/prism/'
} else {
   base.dir <- '/storage/data/climate/downscale/BCCAQ2+PRISM/mvbc/bccaq2/'
}

data.dir <- paste0('/storage/data/climate/downscale/CMIP5_delivery/',gcm,'/lat_split/')

template.dir <- paste0(base.dir,gcm,'/template/',scenario,'/climdex/')
template.file <- list.files(path=template.dir,pattern=paste0(varname,'ETCCDI'))

if (grepl('r9',varname)) {
   template.file <- list.files(path=template.dir,pattern=varname)
}

##Move data to local storage for better I/O
if (!file.exists(tmp.dir)) {
  dir.create(tmp.dir,recursive=TRUE)
}
dir.create(paste0(tmp.dir,scenario,'/climdex/'),recursive=T)

rtm <- proc.time()
print('Copying')
print(template.file)

file.copy(from=paste0(template.dir,template.file),to=paste0(tmp.dir,scenario,'/climdex/'),overwrite=TRUE,recursive=TRUE) ##

print('Move template time')
print(proc.time()-rtm)

##---------------------------------------------------------------------------
if (type=='annual') {
  ##Annual Average Files for writing
  print('Ann Avg opening')
  ann.name <- varname
  ann.dir <- paste0(tmp.dir,scenario,'/climdex/')
  ann.file <- paste0(ann.dir,ann.name,'ETCCDI_ann_BCCAQ2_PRISM_',gcm,'_',scenario,'_',run,'_',interval,'.nc')
  ann.ncs <- nc_open(ann.file,write=TRUE)  
  common.lat <- ncvar_get(ann.ncs,'lat')
}

##---------------------------------------------------------------------------
if (type=='monthly') {
  ##Monthly Average Files for writing
  print('Monthly avg opening')
  mon.name <- varname
  mon.dir <- paste0(tmp.dir,scenario,'/climdex/')
  mon.file <- paste0(mon.dir,mon.name,'ETCCDI_mon_BCCAQ2_PRISM_',gcm,'_',scenario,'_',run,'_',interval,'.nc')
  mon.ncs <- nc_open(mon.file,write=TRUE)
  common.lat <- ncvar_get(mon.ncs,'lat')
}

if (grepl('r9',varname)) {
  ##R95 Files for writing
  print('R9 opening')
  ann.names <- c(paste0(varname,'p'),paste0(varname,'days'),paste0(varname,'store'))
  ann.ncs <- vector(mode='list',length=length(ann.names))
  ann.dir <- paste0(tmp.dir,scenario,'/climdex/')
  ann.files <- paste0(ann.dir,ann.names,'ETCCDI_ann_BCCAQ2_PRISM_',gcm,'_',scenario,'_',run,'_',interval,'.nc')
  for (d in seq_along(ann.names)) {
    ann.ncs[[d]] <- nc_open(ann.files[d],write=TRUE)  
  }
  common.lat <- ncvar_get(ann.ncs[[1]],'lat')
}



##---------------------------------------------------------------------------
##---------------------------------------------------------------------------

##Iterate over the latitude files
for (i in 1:len) {
  lat.interval <- paste0(sprintf('%.1f',lat.st[i]),'-',sprintf('%.1f',lat.en[i]))
  
    data.input <- paste0(input.name,'_gcm_prism_BCCAQ2_',gcm,'_',scenario,'_',run,'_',interval,'_',lat.interval,'.nc')
    print(paste0('Copy ',data.input))

    file.copy(paste0(data.dir,"/",data.input),tmp.dir,overwrite=TRUE)

    print('Data opening')
    input.file <- paste0(input.name,'_gcm_prism_BCCAQ2_',gcm,'_',scenario,'_',run,'_',interval,'_',lat.interval,'.nc')
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
    ltm <- proc.time()

    print(paste0('Latitude: ',j,' of ',n.lat))
    input.subset <- ncvar_get(input.nc,input.name,start=c(1,j,1),count=c(-1,1,-1))
    flag <- is.na(input.subset[,1])
    input.list <- vector(mode='list',length=n.lon)

    rtm <- proc.time()     
    input.list <- lapply(seq_len(nrow(input.subset)), function(k) input.subset[k,])
    rm(input.subset)
    print('Lapply convert to list')
    print(proc.time()-rtm) 

    ##----------------------------------------------------------
    ##Annual Averages 
    if (type=='annual') {
    rtm <- proc.time()     
    annual.averages.for.model(ann.name,ann.ncs,lat.ix,n.lon,yearly.fac,flag,
                              input.list)
    print('Annual Averages Time')
    print(proc.time()-rtm) 
    }
    ##----------------------------------------------------------
    ##Monthly Averages 
    if (type=='monthly') {
    rtm <- proc.time()     
      monthly.averages.for.model(mon.name,mon.ncs,lat.ix,n.lon,monthly.fac,flag,
                                input.list)
    print('Monthly Averages Time')
    print(proc.time()-rtm) 
    }
    ##----------------------------------------------------------
    if (type=='annual_extremes') {
    ##Annual Extremes
    rtm <- proc.time()     
      annual.extremes.for.model(ext.name,ext.ncs,lat.ix,n.lon,yearly.fac,flag,
                                input.list)
    print('Annual Extremes Time')
    print(proc.time()-rtm) 
    }

    if (grepl('r9',varname)) {
    ##Extreme Precip
    rtm <- proc.time()     
      r9.precip.for.model(varname,ann.names,ann.ncs,lat.ix,n.lon,yearly.fac,input.dates,flag,
                                input.list)
    print('R9 Extremes Time')
    print(proc.time()-rtm) 
    }


    ##----------------------------------------------------------
    rm(input.list)

    print('Lon loop time')
    print(proc.time()-ltm)
    
  }##Longitude Loop
  nc_close(input.nc)

  print('Removing lat band files')
  file.remove(paste0(tmp.dir,"/",data.input))

}##Latitude File Loop

##Move back
write.dir <- paste0(base.dir,gcm)
print(write.dir)

file.copy(from=paste0(tmp.dir,scenario,"/climdex/",template.file),to=paste0(write.dir,'/',scenario,'/climdex/'),overwrite=TRUE)


if (type=='annual') {
   nc_close(ann.ncs)
}

if (type=='seasonal') {
    nc_close(seas.ncs)
}

if (type=='monthly') {
  nc_close(mon.ncs)
}

if (grepl('r9',varname)) {
  for (d in seq_along(ann.names)) {
    ann.ncs[[d]] <- nc_open(ann.files[d],write=TRUE)  
  }

}



clean.up <- paste("rm ",tmp.dir,scenario,"/climdex/*",sep='')
print(clean.up)
##system(clean.up)

clean.up <- paste("rmdir ",tmp.dir,scenario,"/climdex",sep='')
print(clean.up)
##system(clean.up)

print("Total Elapsed Time:")
print(proc.time()-ptm)

