##Set up and submit jobs to run BCCS and BCCA for the specified models
##

submit_combine_job <- function(var.name,gcm,run,scenario,type) {

      comb.array <- readLines(comb.template)

      ##COMB
      ##Output located at line 10
      comb.array[6] <- paste0("#PBS -o ./out_files/",gcm,"-",var.name,"-",run,"-comb.out")
      comb.array[7] <- paste0("#PBS -e ./out_files/",gcm,"-",var.name,"-",run,"-comb.err")
      comb.array[8] <- paste0("#PBS -N comb.",tolower(substr(gcm,1,3)),".",substr(run,1,3))

      comb.array[14] <- paste0('gcm="',gcm,'"')
      comb.array[15] <- paste0('run="',run,'"')
      comb.array[16] <- paste0('scenario="',scenario,'"')
      comb.array[17] <- paste0('varname="',var.name,'"')

      comb.file <- paste0("./sub-combine.",var.name,".pbs")
      writeLines(comb.array,comb.file)
      work <- paste0("qsub ",comb.file)
      print(work)
      system(work)
      Sys.sleep(2)

}

##---------------------------------------------------------------------------

gcm.list <- 'CanESM5'
scenario <- 'ssp126'
##'r1i1p2f1','r2i1p2f1',
run.list <- 'r8i1p2f1' ##c('r4i1p2f1','r5i1p2f1','r6i1p2f1','r7i1p2f1')
##              'r8i1p2f1','r9i1p2f1','r10i1p2f1')

var.list <- 'tasmax' ##c('pr','tasmax','tasmin')

##---------------------------------------------------------------------------


comb.template <- "/storage/home/ssobie/code/repos/downscale_CMIP6/template.recombinant.files.pbs"

for (var.name in var.list) {
   for (gcm in gcm.list) {
      for (run in run.list) {              
         submit_combine_job(var.name,gcm,run,scenario)
      }
   }
}




