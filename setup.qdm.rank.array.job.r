##Set up and submit jobs to run BCCS and BCCA for the specified models
##

submit_one_qdm_rank_job <- function(var.name,gcm,run,scenario,index,
                                    obsfile,bccifile,bccafile,
                                    obsdir,bccidir,bccadir,writedir) {
                                     
      job.template <- "/storage/home/ssobie/code/repos/downscale_CMIP6/template.single.qdm.rank.pbs"
      bccs.array <- readLines(job.template)

      ##BCCS
      ##Output located at line 10
      bccs.array[6] <- paste0("#PBS -o output=./",gcm,"-",var.name,"-",run,"-qdrank.",index,".out")
      bccs.array[7] <- paste0("#PBS -e error=./",gcm,"-",var.name,"-",run,"-qdrank.",index,".err")
      bccs.array[8] <- paste0("#PBS -N qdr.",tolower(substr(gcm,1,3)),".",substr(run,1,3),".",index)

      bccs.array[13] <- paste0('gcm="',gcm,'"')
      bccs.array[14] <- paste0('run="',run,'"')
      bccs.array[15] <- paste0('scenario="',scenario,'"')       
      bccs.array[16] <- paste0('index="',index,'"')

      bccs.array[18] <- paste0('varname="',var.name,'"')
      bccs.array[19] <- paste0('obsdir="',obsdir,'"')
      bccs.array[20] <- paste0('bccidir="',bccidir,'"')
      bccs.array[21] <- paste0('bccadir="',bccadir,'"')
      bccs.array[22] <- paste0('bccafile="',bccafile,'"')
      bccs.array[23] <- paste0('writedir="',writedir,'"')

      bccs.array[25] <- paste0('obsfile="',obsfile,'"')
      bccs.array[26] <- paste0('bccifile="',bccifile,'"')

      bccs.file <- paste0("./sub-qdrank.",var.name,".pbs")
      writeLines(bccs.array,bccs.file)
      work <- paste0("qsub ",bccs.file)
      print(work)
      system(work)
      Sys.sleep(2)

}

##---------------------------------------------------------------------------


