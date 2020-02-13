##Script to contain necessary functions for calculating the derived variables
##for the 800m data

##----------------------------------------------------------
##Degree Days

##Degree Day Values
dd <- function(tmean,tbase) {
  g <- tmean - tbase
  days <- sum(g[g>0], na.rm=T)
  return(round(days))
}

fd <- function(tmin) {
  days <- sum(tmin>0,na.rm=T)
  return((days))
}

s3 <- function(tmax) {
  days <- sum(tmax>30,na.rm=T)
  return((days))
}


gdd<-function(data,fac){tapply(data,fac, dd, tbase=5)}   ##Growing degree days
cdd<-function(data,fac){tapply(data,fac, dd, tbase=18)}  ##Cooling degree days
hdd<-function(data,fac){tapply(-data,fac,dd, tbase=-18)} ##Heating degree days
fdd<-function(data,fac){tapply(-data,fac,dd, tbase=0)} ##Freezing degree days
ffd<-function(data,fac){tapply(data,fac,fd)} ##Frost Free days
s30<-function(data,fac){tapply(data,fac,s3)} ##Days over 30 degC

quant.fxn  <- function(data,fac,pctl){tapply(data,fac,quantile,pctl,na.rm=T)}


dd.fxns <- list(gdd=gdd,
                cdd=cdd,
                hdd=hdd,
                fdd=fdd,
                s30=s30)

##----------------------------------------------------------
##Annual Averages 
ann.fxns <- list(tasmax=function(data,fac){tapply(data,fac,mean)},
                 tasmin=function(data,fac){tapply(data,fac,mean)},
                 pr=function(data,fac){tapply(data,fac,sum)},
                 gsl=function(data,fac,dates){growing.season.length(data,fac,dates,northern.hemisphere=TRUE)})


##----------------------------------------------------------
##Seasonal Averages 
seas.fxns <- list(tasmax=function(data,fac){tapply(data,fac,mean)},
                  tasmin=function(data,fac){tapply(data,fac,mean)},
                  pr=function(data,fac){tapply(data,fac,sum)})

##----------------------------------------------------------
##Monthly Averages 
mon.fxns <- list(tasmax=function(data,fac){tapply(data,fac,mean)},
                 tasmin=function(data,fac){tapply(data,fac,mean)},
                 pr=function(data,fac){tapply(data,fac,sum)},
                 dtr=function(data,fac){tapply(data,fac,mean)})

##----------------------------------------------------------
##Annual Extremes
ext.fxns <- list(tasmax=function(data,fac){tapply(data,fac,max)},
                 tasmin=function(data,fac){tapply(data,fac,min)},
                 pr=function(data,fac){tapply(data,fac,max)})

##----------------------------------------------------------
##Annual Quantiles



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
                        climdex.r95ptot=c('r95pETCCDI','Ann','mm'),
                        climdex.r99ptot=c('r99pETCCDI','Ann','mm'),
                        climdex.prcptot=c('prcptotETCCDI','Ann','mm'),
                        climdex.r95days=c('r95daysETCCDI','Ann','days'),
                        climdex.r99days=c('r99daysETCCDI','Ann','days'),
                        climdex.r95store=c('r95storeETCCDI','Ann','days'),
                        climdex.r99store=c('r99storeETCCDI','Ann','days'))

  rv <- climdex.names[[climdex.name]]
  return(rv)
}


##-----------------------------------------------
##Extra Climdex Functions

climdex.r95days <- function(clim.object) {

  years <- as.Date(paste(unique(format(clim.object@dates,'%Y')),'-01-16',sep=''))
  na.rv <- rep(NA,length(years))
  precip <- clim.object@data$prec
  if (sum(is.na(precip))==length(precip))
    return(na.rv)
  q.obj <- get.outofbase.quantiles(prec=clim.object@data$prec,prec.dates=clim.object@dates,
                                                 base=c(1951,1954))
  q.95 <- number.days.op.threshold(clim.object@data$prec,date.factor=clim.object@date.factors$annual, q.obj$prec[1], op = ">")

  return(q.95)
}

climdex.r99days <- function(clim.object) {

  years <- as.Date(paste(unique(format(clim.object@dates,'%Y')),'-01-16',sep=''))
  na.rv <- rep(NA,length(years))
  precip <- clim.object@data$prec
  if (sum(is.na(precip))==length(precip))
    return(na.rv)
  q.obj <- get.outofbase.quantiles(prec=clim.object@data$prec,prec.dates=clim.object@dates,
                                                 base=c(1951,1954))
  q.99 <- number.days.op.threshold(clim.object@data$prec,date.factor=clim.object@date.factors$annual, q.obj$prec[2], op = ">")
  return(q.99)
}

climdex.r95store <- function(clim.object) {

  years <- as.Date(paste(unique(format(clim.object@dates,'%Y')),'-01-16',sep=''))
  q.obj <- get.outofbase.quantiles(prec=clim.object@data$prec,prec.dates=clim.object@dates,
                                                 base=c(1951,1954))
  r95 <- q.obj$prec[1]
  r95.rv <- rep(r95,length(years))
  return(r95.rv)
}

climdex.r99store <- function(clim.object) {

  years <- as.Date(paste(unique(format(clim.object@dates,'%Y')),'-01-16',sep=''))
  q.obj <- get.outofbase.quantiles(prec=clim.object@data$prec,prec.dates=clim.object@dates,
                                                 base=c(1951,1954))
  r99 <- q.obj$prec[2]
  r99.rv <- rep(r99,length(years))
  return(r99.rv)
}

climdex.heatwave <- function(ci) {
    spells.can.span.years <- FALSE
    stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax))
    return(threshold.exceedance.duration.index(ci@data$tmax,
        ci@date.factors$monthly, ci@jdays, ci@quantiles$tmax$outbase$q90,
        ">", min.length=3,spells.can.span.years = spells.can.span.years, max.missing.days = ci@max.missing.days["monthly"]) *
        ci@namasks$annual$tmax)
}

climdex.rx2day <- function(ci, freq="monthly",center.mean.on.last.day = FALSE)
{
    stopifnot(!is.null(ci@data$prec))
    return(nday.consec.prec.max(ci@data$prec, ci@date.factors[[match.arg(freq)]],
        2, center.mean.on.last.day) * ci@namasks[[match.arg(freq)]]$prec)
}

