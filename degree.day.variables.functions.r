##Script to calculate and write the standard set of derived variables
##for the CMIP6 BCCAQv2

degree_days_for_model <- function(degree.names,dd.ncs,lat.ix,n.lon,flag,
                                  tas.list,yearly.fac) {

    flen <- sum(!flag)
    n.col <- length(levels(yearly.fac))

    for (k in seq_along(degree.names)) {
       degree.matrix <- matrix(NA,nrow=n.lon,ncol=n.col) 
       if (flen!=0) { ##Some Real Values
          sub.list <- tas.list[!flag]
          print(degree.names[k])
          degree.name <- degree.names[k]
          dd.nc <- dd.ncs[[k]]
          fx <- dd.fxns[[degree.names[k]]]
          sub.values <- foreach(
                         temp=sub.list,
                         .export=c('yearly.fac','fx','dd')
                         ) %do% {
                              degree.values <- fx(temp,yearly.fac)
                         }
          rm(sub.list)
          sub.matrix <- matrix(unlist(sub.values),nrow=flen,ncol=n.col,byrow=TRUE)
          rm(sub.values)
          degree.matrix[!flag,] <- sub.matrix
          rm(sub.matrix)
       } else {
          print('All NA values')
       }
       print(dim(degree.matrix))
       
       ncvar_put(dd.ncs[[k]],varid=degree.names[k],vals=degree.matrix,
                 start=c(1,lat.ix,1),count=c(-1,1,-1))
       rm(degree.matrix)       
       gc()
    } ##Degree day loop
}


##----------------------------------------------------------------------------------------------

gsl_for_model <- function(ann.name,ann.ncs,lat.ix,n.lon,flag,
                          ann.list,yearly.fac,dates) {
   ##Variables
       flen <- sum(!flag)
       n.col <- length(levels(yearly.fac))
       ann.avg.matrix <- matrix(NA,nrow=n.lon,ncol=n.col)

       if (flen!=0) { ##Some Real Values
          ann.fx <- ann.fxns[[ann.name]]
          sub.list <- ann.list[!flag]
          rm(ann.list)

          ann.avg.values <- foreach(
                            data=sub.list,
                            .export=c('yearly.fac','ann.fx','dates')
                            ) %do% {
                                 ann.avg.values <- ann.fx(data,yearly.fac,dates)
                            }
         rm(sub.list)
         sub.matrix <- matrix(unlist(ann.avg.values),nrow=flen,ncol=n.col,byrow=TRUE)
         rm(ann.avg.values)
         ann.avg.matrix[!flag] <- sub.matrix
         rm(sub.matrix)
      } else {
         print('All NA values')
      }
      ncvar_put(ann.ncs,varid=paste0(ann.name,'ETCCDI'),vals=ann.avg.matrix,
                start=c(1,lat.ix,1),count=c(-1,1,-1))
      rm(ann.avg.matrix)
      gc()
}

##----------------------------------------------------------------------------------------------
dtr_for_model <- function(mon.name,mon.ncs,lat.ix,n.lon,flag,
                                   mon.list,monthly.fac) {

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

