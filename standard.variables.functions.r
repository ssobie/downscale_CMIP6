##Script to contain the standard variable functions


get_seasonal_fac <- function(seas.dates) {
   seasons <-  c("DJF", "DJF", "MAM", "MAM", "MAM", "JJA", "JJA", "JJA", "SON", "SON", "SON", "DJF")
   years <- as.numeric(format(seas.dates,'%Y'))
   uni.yrs <- unique(years)
   months <- as.numeric(format(seas.dates,'%m'))
   dec.ix <- grep(12,months)
   years[dec.ix] <- years[dec.ix] + 1
   dec.fix <- years %in% uni.yrs
   yearly.fac <- as.factor(years[dec.fix])
   monthly.fac <- as.factor(format(seas.dates[dec.fix],'%m'))      
   seasonal.fac <- factor(seasons[monthly.fac], levels=c('DJF', 'MAM', 'JJA', 'SON'))
   avg.fac <- list(yearly.fac,seasonal.fac)       

  return(list(fac=avg.fac,fix=dec.fix))
}

##----------------------------------------------------------------------------------------------
annual_averages_for_model <- function(ann.name,ann.ncs,lat.ix,n.lon,yearly.fac,flag,
                                      ann.list) {                                            
   ##Variables
       flen <- sum(!flag)
       n.col <- length(levels(yearly.fac))
       ann.avg.matrix <- matrix(NA,nrow=n.lon,ncol=n.col)

       if (flen!=0) { ##Some Real Values
          ann.fx <- ann.fxns[[ann.name]]
          sub.list <- ann.list[!flag]
          ann.avg.values <- foreach(
                            data=sub.list,
                            .export=c('yearly.fac','ann.fx')
                            ) %do% {
                                 ann.avg.values <- ann.fx(data,yearly.fac)
                            }
         sub.matrix <- matrix(unlist(ann.avg.values),nrow=flen,ncol=n.col,byrow=TRUE)
         rm(ann.avg.values)
         ann.avg.matrix[!flag] <- sub.matrix
         rm(sub.matrix)
         rm(sub.list)                         
      } else {
         print('All NA values')
      }
      rm(ann.list)

      ncvar_put(ann.ncs,varid=ann.name,vals=ann.avg.matrix,
                start=c(1,lat.ix,1),count=c(-1,1,-1))
      rm(ann.avg.matrix)                
      gc()
}

##----------------------------------------------------------------------------------------------
seasonal_averages_for_model <- function(seas.name,seas.ncs,lat.ix,n.lon,seasonal.fac,flag,
                                        seas.list) {

       ##Roll over the December months to compute proper seasons
       flen <- sum(!flag)
       n.col <- length(levels(seasonal.fac$fac[[1]]))  * length(levels(seasonal.fac$fac[[2]]))
       seas.avg.matrix <- matrix(NA,nrow=n.lon,ncol=n.col)

       if (flen!=0) { ##Some Real Values
          seas.fx <- seas.fxns[[seas.name]]
          avg.fac <- seasonal.fac$fac
          dec.fix <- seasonal.fac$fix
          seas.corr.list <- lapply(seas.list,function(x,y){x[y]},dec.fix)
          rm(seas.list)
          sub.list <- seas.corr.list[!flag]
          rm(seas.corr.list)
          seas.avg.values <- foreach(
                            data=sub.list,
                            .export=c('avg.fac','seas.fx')
                            ) %do% {
                                 seas.avg.values <- as.vector(t(seas.fx(data,avg.fac)))
                            }
          rm(sub.list)                         		 
          sub.matrix <- matrix(unlist(seas.avg.values),nrow=flen,ncol=n.col,byrow=TRUE)
          rm(seas.avg.values)
          seas.avg.matrix[!flag,] <- sub.matrix
          rm(sub.matrix)
      } else {
          print('All NA values')
      }
      ncvar_put(seas.ncs,varid=seas.name,vals=seas.avg.matrix,
                start=c(1,lat.ix,1),count=c(-1,1,-1))
      rm(seas.avg.matrix)          
      gc()
}

##----------------------------------------------------------------------------------------------
monthly_averages_for_model <- function(mon.name,mon.ncs,lat.ix,n.lon,monthly.fac,flag,
                                       mon.list) {
      
   ##Variables
   flen <- sum(!flag)
   n.col <- length(levels(monthly.fac))
   mon.avg.matrix <- matrix(NA,nrow=n.lon,ncol=n.col)

   if (flen!=0) { ##Some Real Values
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
      sub.matrix <- matrix(unlist(mon.avg.values),nrow=flen,ncol=n.col,byrow=TRUE)
      rm(mon.avg.values)
      mon.avg.matrix[!flag,] <- sub.matrix
      rm(sub.matrix)
   } else {
      print('All NA values')
   }
   ncvar_put(mon.ncs,varid=mon.name,vals=mon.avg.matrix,
            start=c(1,lat.ix,1),count=c(-1,1,-1))
   rm(mon.avg.matrix)
   gc()
}

##----------------------------------------------------------------------------------------------
annual_extremes_for_model <- function(ext.name,ext.ncs,lat.ix,n.lon,yearly.fac,flag,
                                      ext.list) {
      
   ##Variables
   flen <- sum(!flag)
   n.col <- length(levels(yearly.fac))
   ext.avg.matrix <- matrix(NA,nrow=n.lon,ncol=n.col)

   if (flen!=0) { ##Some Real Values
      sub.list <- ext.list[!flag]
      rm(ext.list)
      ext.fx <- ext.fxns[[ext.name]]

      ext.avg.values <- foreach(
                                data=sub.list,
                                .export=c('yearly.fac','ext.fx')
                                ) %do% {
                                     ext.avg.values <- ext.fx(data,yearly.fac)
                                }
      rm(sub.list)                         
      sub.matrix <- matrix(unlist(ext.avg.values),nrow=flen,ncol=n.col,byrow=TRUE)
      rm(ext.avg.values)
      ext.avg.matrix[!flag,] <- sub.matrix
      rm(sub.matrix)
   } else {
      print('All NA Values')
   }
   ncvar_put(ext.ncs,varid=ext.name,vals=ext.avg.matrix,
             start=c(1,lat.ix,1),count=c(-1,1,-1))
   rm(ext.avg.matrix)		
}


##----------------------------------------------------------------------------------------------
annual_quantiles_for_model <- function(quant.name,quant.value,quant.ncs,lat.ix,n.lon,yearly.fac,flag,
                                       quant.list) {

   flen <- sum(!flag)     
   n.col <- length(levels(yearly.fac))
   quant.avg.matrix <- matrix(NA,nrow=n.lon,ncol=n.col)

   if (flen!=0) { ##Some Real Values
      sub.list <- quant.list[!flag] 
      quant.fx  <- function(data,fac,pctl){tapply(data,fac,quantile,pctl,na.rm=T,names=FALSE)}

      ##Variables
      quant.val <- as.numeric(quant.value)/1000
      print(quant.val)
      quant.avg.values <- foreach(
                              data=sub.list,
                              .export=c('yearly.fac','quant.fx','quant.val')
                              ) %do% {
                                   quant.avg.values <- quant.fx(data,yearly.fac,quant.val)
                              }
      sub.matrix <- matrix(unlist(quant.avg.values),nrow=flen,ncol=n.col,byrow=TRUE)
      rm(quant.avg.values)
      quant.avg.matrix[!flag,] <- sub.matrix
      rm(sub.matrix)
   } else { 
      print('All NA Values')
   }
   ncvar_put(quant.ncs,varid=quant.name,vals=quant.avg.matrix,
             start=c(1,lat.ix,1),count=c(-1,1,-1))
   rm(quant.avg.matrix)		
   gc()
}

