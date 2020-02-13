##Script to contain necessary functions for calculating the derived variables
##for the 800m data
library(climdex.pcic)
##----------------------------------------------------------

r95ptotal <- function(data,fac,dates) {
  qtile <- get.outofbase.quantiles(prec=data,prec.dates=dates,prec.qtiles = 0.95,base.range=c(1971,2000))
  r95p <- total.precip.op.threshold(data, fac, qtile$prec, ">")
  r95days <- number.days.op.threshold(data,fac, qtile$prec, op = ">")  
  r95store <- rep(qtile$prec,length(levels(fac)))
  rv <- list(total=r95p,days=r95days,store=r95store)
  return(rv)
}

r99ptotal <- function(data,fac,dates) {
  qtile <- get.outofbase.quantiles(prec=data,prec.dates=dates,prec.qtiles = 0.99,base.range=c(1971,2000))
  r99p <- total.precip.op.threshold(data, fac, qtile$prec, ">")
  r99days <- number.days.op.threshold(data,fac, qtile$prec, op = ">")  
  r99store <- rep(qtile$prec,length(levels(fac)))
  rv <- list(total=r99p,days=r99days,store=r99store)
  return(rv)
}



##----------------------------------------------------------
##Annual Averages 
ann.fxns <- list(su=function(data,fac){number.days.op.threshold(data, fac,25, ">")},
                 su30=function(data,fac){number.days.op.threshold(data, fac,30, ">")},
                 id=function(data,fac){number.days.op.threshold(data, fac,0, "<")},
                 fd=function(data,fac){number.days.op.threshold(data, fac,0, "<")},
                 tr=function(data,fac){number.days.op.threshold(data, fac,20, ">")},
                 gsl=function(data,fac,dates){growing.season.length(data,fac,dates)},
                 r10mm=function(data,fac){number.days.op.threshold(data, fac,10, ">")},
                 r20mm=function(data,fac){number.days.op.threshold(data, fac,20, ">")},
                 sdii=function(data,fac){simple.precipitation.intensity.index(data,fac)},
                 prcptot=function(data,fac){total.precip.op.threshold(data,fac,1, ">=")},
                 cwd=function(data,fac){spell.length.max(data,fac,1, ">=", spells.can.span.years=TRUE)},
                 cdd=function(data,fac){spell.length.max(data,fac,1, "<", spells.can.span.years=TRUE)},
                 r95=function(data,fac,dates){r95ptotal(data,fac,dates)},
                 r99=function(data,fac,dates){r99ptotal(data,fac,dates)})

##----------------------------------------------------------
##Monthly Averages 
mon.fxns <- list(txx=function(data,fac){tapply(data,fac,max)},
                 txn=function(data,fac){tapply(data,fac,min)},
                 tnn=function(data,fac){tapply(data,fac,min)},
                 tnx=function(data,fac){tapply(data,fac,max)},
                 dtr=function(data,fac){tapply(data,fac,mean)},
                 rx1day=function(data,fac){nday.consec.prec.max(data, fac,1)},
                 rx2day=function(data,fac){nday.consec.prec.max(data, fac,2)},
                 rx5day=function(data,fac){nday.consec.prec.max(data, fac,5)})

##----------------------------------------------------------
##Annual Extremes
ext.fxns <- list(tasmax=function(data,fac){tapply(data,fac,max)},
                 tasmin=function(data,fac){tapply(data,fac,min)},
                 pr=function(data,fac){tapply(data,fac,max)})



get.climdex.info <- function(climdex.name) {

  climdex.names <- list(climdex.fd=c('fdETCCDI','annual','days'),
                        climdex.su=c('suETCCDI','annual','days'),
                        climdex.id=c('idETCCDI','annual','days'),
                        climdex.tr=c('trETCCDI','annual','days'),
                        climdex.gsl=c('gslETCCDI','annual','days'),
                        climdex.txx=c('txxETCCDI','monthly','degC'),
                        climdex.tnx=c('tnxETCCDI','monthly','degC'),
                        climdex.txn=c('txnETCCDI','monthly','degC'),
                        climdex.tnn=c('tnnETCCDI','monthly','degC'),
                        climdex.tn10p=c('tn10pETCCDI','monthly','days'),
                        climdex.tx10p=c('tx10pETCCDI','monthly','days'),
                        climdex.tn90p=c('tn90pETCCDI','monthly','days'),
                        climdex.tx90p=c('tx90pETCCDI','monthly','days'),
                        climdex.wsdi=c('wsdiETCCDI','annual','days'),
                        climdex.csdi=c('csdiETCCDI','annual','days'),
                        climdex.dtr=c('dtrETCCDI','monthly','degC'),
                        climdex.rx1day=c('rx1dayETCCDI','monthly','mm'),
                        climdex.rx2day=c('rx2dayETCCDI','monthly','mm'),
                        climdex.rx5day=c('rx5dayETCCDI','monthly','mm'),
                        climdex.sdii=c('sdiiETCCDI','annual','mm d-1'),
                        climdex.r10mm=c('r10mmETCCDI','annual','days'),
                        climdex.r20mm=c('r20mmETCCDI','annual','days'),
                        climdex.cdd=c('cddETCCDI','annual','days'),
                        climdex.cwd=c('cwdETCCDI','annual','days'),
                        climdex.r95p=c('r95pETCCDI','annual','mm'),
                        climdex.r99p=c('r99pETCCDI','annual','mm'),
                        climdex.prcptot=c('prcptotETCCDI','annual','mm'),
                        climdex.r95days=c('r95daysETCCDI','annual','days'),
                        climdex.r99days=c('r99daysETCCDI','annual','days'),
                        climdex.r95store=c('r95storeETCCDI','annual','days'),
                        climdex.r99store=c('r99storeETCCDI','annual','days'))

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
                                                 base=c(1971,2000))
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
                                                 base=c(1971,2000))
  q.99 <- number.days.op.threshold(clim.object@data$prec,date.factor=clim.object@date.factors$annual, q.obj$prec[2], op = ">")
  return(q.99)
}

climdex.r95store <- function(clim.object) {

  years <- as.Date(paste(unique(format(clim.object@dates,'%Y')),'-01-16',sep=''))
  q.obj <- get.outofbase.quantiles(prec=clim.object@data$prec,prec.dates=clim.object@dates,
                                                 base=c(1971,2000))
  r95 <- q.obj$prec[1]
  r95.rv <- rep(r95,length(years))
  return(r95.rv)
}

climdex.r99store <- function(clim.object) {

  years <- as.Date(paste(unique(format(clim.object@dates,'%Y')),'-01-16',sep=''))
  q.obj <- get.outofbase.quantiles(prec=clim.object@data$prec,prec.dates=clim.object@dates,
                                                 base=c(1971,2000))
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

clim.fxns <- list(climdex.fd=climdex.fd,
                  climdex.su=climdex.su,
                  climdex.id=climdex.id,
                  climdex.tr=climdex.tr,
                  climdex.gsl=climdex.gsl,
                  climdex.txx=climdex.txx,
                  climdex.tnx=climdex.tnx,
                  climdex.txn=climdex.txn,
                  climdex.tnn=climdex.tnn,
                  climdex.tn10p=climdex.tn10p,
                  climdex.tx10p=climdex.tx10p,
                  climdex.tn90p=climdex.tn90p,
                  climdex.tx90p=climdex.tx90p,
                  climdex.wsdi=climdex.wsdi,
                  climdex.csdi=climdex.csdi,
                  climdex.dtr=climdex.dtr,
                  climdex.rx1day=climdex.rx1day,
                  climdex.rx2day=climdex.rx2day,
                  climdex.rx5day=climdex.rx5day,
                  climdex.sdii=climdex.sdii,
                  climdex.r10mm=climdex.r10mm,
                  climdex.r20mm=climdex.r20mm,
                  climdex.cdd=climdex.cdd,
                  climdex.cwd=climdex.cwd,
                  climdex.r95p=climdex.r95ptot,
                  climdex.r99p=climdex.r99ptot,
                  climdex.prcptot=climdex.prcptot,
                  climdex.r95days=climdex.r95days,
                  climdex.r99days=climdex.r99days,
                  climdex.r95store=climdex.r95store,
                  climdex.r99store=climdex.r99store)


input.varname <- list(fd='tasmin',
                      su='tasmax',
                      su30='tasmax',
                      id='tasmax',
                      tr='tasmin',
                      gsl='tas',
                      txx='tasmax',
                      txn='tasmax',
                      tnn='tasmin',
                      tnx='tasmin',                      
                  climdex.tn10p=climdex.tn10p,
                  climdex.tx10p=climdex.tx10p,
                  climdex.tn90p=climdex.tn90p,
                  climdex.tx90p=climdex.tx90p,
                  climdex.wsdi=climdex.wsdi,
                  climdex.csdi=climdex.csdi,
                      dtr='tasdiff',
                      rx1day='pr',
                      rx2day='pr',
                      rx5day='pr',
                      sdii='pr',
                      r10mm='pr',
                      r20mm='pr',
                      cdd='pr',
                      cwd='pr',
                      r95='pr',
                      r99='pr',
                      prcptot='pr')


