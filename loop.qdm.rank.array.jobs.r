##Set up and submit jobs to run BCCS and BCCA for the specified models
##

submit_qdm_rank_job <- function(var.name,gcm,run,scenario,type,
                                bccafile,
                                obsdir,bccidir,bccadir,writedir,   
                                bccs.template) {     

      bccs.array <- readLines(bccs.template)

      ##BCCS
      ##Output located at line 10
      bccs.array[7] <- paste0("#PBS -o output=./",gcm,"-",var.name,"-",run,"-",type,".out")
      bccs.array[8] <- paste0("#PBS -e error=./",gcm,"-",var.name,"-",run,"-",type,".err")
      bccs.array[9] <- paste0("#PBS -N ",type,".",tolower(substr(gcm,1,3)),".",substr(run,1,3))

      bccs.array[14] <- paste0('gcm="',gcm,'"')
      bccs.array[15] <- paste0('run="',run,'"')
      bccs.array[16] <- paste0('scenario="',scenario,'"')

      bccs.array[18] <- paste0('varname="',var.name,'"')
      bccs.array[19] <- paste0('obsdir="',obsdir,'"')
      bccs.array[20] <- paste0('bccidir="',bccidir,'"')
      bccs.array[21] <- paste0('bccadir="',bccadir,'"')
      bccs.array[22] <- paste0('bccafile="',bccafile,'"')
      bccs.array[23] <- paste0('writedir="',writedir,'"')

      bccs.file <- paste0("./sub-",type,".",var.name,".pbs")
      writeLines(bccs.array,bccs.file)
      work <- paste0("qsub ",bccs.file)
      print(work)
      system(work)
      Sys.sleep(2)

}

##---------------------------------------------------------------------------

bccadir <- '/storage/data/climate/downscale/BCCAQ2/BCCA/'


##---------------------------------------------------------------------------

gcm.list <- 'CanESM5'
scenario <- 'ssp585'
## ##,
run.list <- c('r3i1p2f1','r4i1p2f1','r5i1p2f1','r6i1p2f1','r7i1p2f1','r8i1p2f1','r9i1p2f1')
##,'r7i1p2f1','r8i1p2f1') ##,
##              'r10i1p2f1')
var.list <- 'tasmin'

##---------------------------------------------------------------------------


qdm.rank.template <- "/storage/home/ssobie/code/repos/downscale_CMIP6/template.submit.qdm.rank.pbs"


for (var.name in var.list) {
   obsdir <- paste0('/storage/data/climate/observations/gridded/ANUSPLIN/ANUSPLIN_300ARCSEC/anusplin_',var.name,'_split/')
   obs.file <- list.files(path=obsdir,pattern=var.name)
   for (gcm in gcm.list) {
      for (run in run.list) {              
         bccidir <- paste0('/storage/data/climate/downscale/BCCAQ2/BCCI/',var.name,'_',gcm,'_',run,'_',scenario,'_split15/')
         writedir <- paste0('/storage/data/climate/downscale/BCCAQ2/raw_downscaled/ds_',var.name,'_',gcm,'_',run,'_',scenario,'_split15/')
         if (!file.exists(writedir)) {
            dir.create(writedir,recursive=TRUE)
         }
         bccafile <- paste(gcm,'.',var.name,'.',scenario,'.',run,'.Canada.analogues.RData',sep='')

         submit_qdm_rank_job(var.name,gcm,run,scenario,'qdrank',
                             bccafile,
                             obsdir,bccidir,bccadir,writedir,   
                             qdm.rank.template)
      }
   }
}




