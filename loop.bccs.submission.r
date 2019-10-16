##Set up and submit jobs to run BCCS and BCCA for the specified models
##

submit_bccs_job <- function(var.name,gcm,run,scenario,type,
                            gcm.file,obs.file,bccs.file,
                            gcmdir,obsdir,writedir,
                            bccs.template) {     

      bccs.array <- readLines(bccs.template)

      ##BCCS
      ##Output located at line 10
      bccs.array[6] <- paste0("#PBS -o output=./out_files/",gcm,"-",var.name,"-",run,"-",type,".out")
      bccs.array[7] <- paste0("#PBS -e error=./out_files/",gcm,"-",var.name,"-",run,"-",type,".err")
      bccs.array[8] <- paste0("#PBS -N ",type,".",tolower(substr(gcm,1,3)),".",substr(run,1,3))

      bccs.array[14] <- paste0('gcm="',gcm,'"')
      bccs.array[15] <- paste0('run="',run,'"')
      bccs.array[16] <- paste0('scenario="',scenario,'"')

      bccs.array[18] <- paste0('varname="',var.name,'"')
      bccs.array[19] <- paste0('gcmfile="',gcm.file,'"')
      bccs.array[20] <- paste0('gcmdir="',gcmdir,'"')
      bccs.array[21] <- paste0('obsfile="',obs.file,'"')
      bccs.array[22] <- paste0('obsdir="',obsdir,'"')
      bccs.array[23] <- paste0(type,'file="',bccs.file,'"')
      bccs.array[24] <- paste0('writedir="',writedir,toupper(type),'/"')

      bccs.file <- paste0("./sub-",type,".",var.name,".pbs")
      writeLines(bccs.array,bccs.file)
      work <- paste0("qsub ",bccs.file)
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
run.list <- c('r1i1p2f1','r2i1p2f1') ### c('r4i1p2f1','r5i1p2f1',
              ###'r6i1p2f1','r7i1p2f1','r8i1p2f1','r9i1p2f1','r10i1p2f1')
var.list <- 'pr' ##'tasmax',

##---------------------------------------------------------------------------


bcci.template <- "/storage/home/ssobie/code/repos/downscale_CMIP6/template.submit.bcci.pbs"
bcca.template <- "/storage/home/ssobie/code/repos/downscale_CMIP6/template.submit.bcca.pbs"

for (var.name in var.list) {
   obs.file <- paste0('anusplin_',var.name,'_final.nc')
   print(obs.file)
   for (gcm in gcm.list) {
      for (run in run.list) {              
         gcm.pattern <- paste0(var.name,'_day_',gcm,'_North_America_historical\\+',scenario,'_',run,'*')
         gcm.file <- list.files(path=gcmdir,pattern=gcm.pattern)

         bcci.file <- gsub(var.name,paste0(var.name,'_BCCI'),gcm.file)
         submit_bccs_job(var.name,gcm,run,scenario,'bcci',
                         gcm.file,obs.file,bcci.file,
                         gcmdir,obsdir,writedir,   
                         bcci.template)

         bcca.file <- paste(gcm,'.',var.name,'.',scenario,'.',run,'.Canada.analogues.RData',sep='')

         ##submit_bccs_job(var.name,gcm,run,scenario,'bcca',
         ##                gcm.file,obs.file,bcca.file,
         ##                gcmdir,obsdir,writedir,   
         ##                bcca.template)
      }
   }
}




