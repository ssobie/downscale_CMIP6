##Script to calculate and write the standard set of derived variables
##for the 800m data

##----------------------------------------------------------------------------------------------
annual_climdex_for_model <- function(ann.name,ann.ncs,lat.ix,n.lon,yearly.fac,flag,
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
      ncvar_put(ann.ncs,varid=paste0(ann.name,'ETCCDI'),vals=ann.avg.matrix,
                start=c(1,lat.ix,1),count=c(-1,1,-1))
      rm(ann.avg.matrix)                
      gc()
}

##----------------------------------------------------------------------------------------------

monthly_climdex_for_model <- function(mon.name,mon.ncs,lat.ix,n.lon,monthly.fac,flag,
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

   ncvar_put(mon.ncs,varid=paste0(mon.name,'ETCCDI'),vals=mon.avg.matrix,
             start=c(1,lat.ix,1),count=c(-1,1,-1))
   rm(mon.avg.matrix)
   gc()
}

##----------------------------------------------------------------------------------------------
r9_precip_for_model <- function(varname,ann.names,ann.ncs,lat.ix,n.lon,yearly.fac,dates,flag,
                                      ann.list) {
      
   ##Variables
   flen <- sum(!flag)
   n.col <- length(levels(yearly.fac))

   totals.matrix <- days.matrix <- store.matrix <- matrix(NA,nrow=n.lon,ncol=n.col)

   if (flen!=0) { ##Some Real Values
      sub.list <- ann.list[!flag]
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

      sub.totals <- matrix(unlist(totals),nrow=flen,ncol=n.col,byrow=TRUE)
      rm(totals)
      totals.matrix[!flag,] <- sub.totals

      sub.days <- matrix(unlist(days),nrow=flen,ncol=n.col,byrow=TRUE)
      rm(days)
      days.matrix[!flag,] <- sub.days

      sub.store <- matrix(unlist(stores),nrow=flen,ncol=n.col,byrow=TRUE)
      rm(stores)
      store.matrix[!flag,] <- sub.store
   } else {
      print('All NA values')
   }

   ncvar_put(ann.ncs$total,varid=paste0(ann.names[1],'ETCCDI'),vals=totals.matrix,
         start=c(1,lat.ix,1),count=c(-1,1,-1))
   rm(totals.matrix)

   ncvar_put(ann.ncs$days,varid=paste0(ann.names[2],'ETCCDI'),vals=days.matrix,
          start=c(1,lat.ix,1),count=c(-1,1,-1))
   rm(days.matrix)
 
   ncvar_put(ann.ncs$store,varid=paste0(ann.names[3],'ETCCDI'),vals=store.matrix,
             start=c(1,lat.ix,1),count=c(-1,1,-1))
   rm(store.matrix)
   gc()
}

##--------------------------------------------------------------
##****************************************************************

