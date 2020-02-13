##Set up and submit jobs to run BCCS and BCCA for the specified models
##

submit_all_job <- function(var.name,gcm,run,scenario,type,
                           gcm.file,obs.file,bcci.file,bcca.file,
                           gcmdir,obsdir,writedir,
                           all.template) {     

      all.array <- readLines(all.template)

      ##ALL
      ##Output located at line 10
      all.array[6] <- paste0("#PBS -o output=./out_files/",gcm,"-",var.name,"-",run,"-",type,".out")
      all.array[7] <- paste0("#PBS -e error=./out_files/",gcm,"-",var.name,"-",run,"-",type,".err")
      all.array[8] <- paste0("#PBS -N ",type,".",tolower(substr(gcm,1,3)),".",substr(run,1,3))

      all.array[14] <- paste0('gcm="',gcm,'"')
      all.array[15] <- paste0('run="',run,'"')
      all.array[16] <- paste0('scenario="',scenario,'"')

      all.array[18] <- paste0('varname="',var.name,'"')
      all.array[19] <- paste0('gcmfile="',gcm.file,'"')
      all.array[20] <- paste0('gcmdir="',gcmdir,'"')
      all.array[21] <- paste0('obsfile="',obs.file,'"')
      all.array[22] <- paste0('obsdir="',obsdir,'"')
      all.array[23] <- paste0('bccifile="',bcci.file,'"')
      all.array[24] <- paste0('bccafile="',bcca.file,'"')
      all.array[25] <- paste0('writedir="',writedir,'"')

      all.file <- paste0("./sub-",type,".",var.name,".pbs")
      writeLines(all.array,all.file)
      work <- paste0("qsub ",all.file)
      print(work)
      system(work)
      Sys.sleep(2)

}

##---------------------------------------------------------------------------

obsdir <- '/storage/data/climate/observations/gridded/ANUSPLIN/ANUSPLIN_300ARCSEC/'
gcmdir <- '/storage/data/climate/CMIP6/assembled/CanESM5/north_america/'
writedir <- '/storage/data/climate/downscale/BCCAQ2/'

##---------------------------------------------------------------------------

gcm.list <- 'CanESM5'
scenario <- 'ssp585'
## ##)##,


##run.list <- c('r1i1p2f1','r2i1p2f1','r3i1p2f1','r4i1p2f1','r5i1p2f1',
##              'r6i1p2f1','r7i1p2f1','r8i1p2f1','r9i1p2f1','r10i1p2f1')
run.list <- c('r1i1p2f1') ##,'r2i1p2f1') ### c('r4i1p2f1','r5i1p2f1',
              ###'r6i1p2f1','r7i1p2f1','r8i1p2f1','r9i1p2f1','r10i1p2f1')
var.list <- 'pr' ##'tasmax',

##---------------------------------------------------------------------------


all.template <- "/storage/home/ssobie/code/repos/downscale_CMIP6/template.submit.all.pbs"

for (var.name in var.list) {
   obs.file <- paste0('anusplin_',var.name,'_final.nc')
   print(obs.file)
   for (gcm in gcm.list) {
      for (run in run.list) {              
         gcm.pattern <- paste0(var.name,'_day_',gcm,'_North_America_historical\\+',scenario,'_',run,'*')
         gcm.file <- list.files(path=gcmdir,pattern=gcm.pattern)

         bcci.file <- gsub(var.name,paste0(var.name,'_BCCI'),gcm.file)
         bcca.file <- paste0(gcm,'.',var.name,'.',scenario,'.',run,'.Canada.analogues.RData')
         submit_all_job(var.name,gcm,run,scenario,'all',
                         gcm.file,obs.file,bcci.file,bcca.file,
                         gcmdir,obsdir,writedir,   
                         all.template)

      }
   }
}




