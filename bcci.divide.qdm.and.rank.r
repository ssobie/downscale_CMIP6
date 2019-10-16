##Run CI.R from hClimDown and divide up the BCCI output file into
##10 cell latitude bands

library(hClimDown)

source('/storage/home/ssobie/code/repos/downscale_CMIP6/split.latitude.time.series.r')
source('/storage/home/ssobie/code/repos/downscale_CMIP6/setup.qdm.rank.array.job.r')

##--------------------------------------------------------------

print('BCCI Started')

ptm <- proc.time()

args <- commandArgs(trailingOnly=TRUE)
for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
}

options(max.GB=1)
options(calibration.start=as.POSIXct('1950-01-01', tz='GMT'))
options(calibration.end=as.POSIXct('2014-12-31', tz='GMT'))

tmpdir <- paste0(tmpdir,'/bccaq_',gcm,'_',varname,'_',run,'_',scenario,'_tmp')
if (!file.exists(tmpdir)) {
   dir.create(tmpdir,recursive=TRUE)
}

##Move files to local storage
print(tmpdir)
print(gcmdir)
print(gcmfile)

file.copy(from=paste0(gcmdir,gcmfile),to=tmpdir,overwrite=TRUE)
file.copy(from=paste0(obsdir,obsfile),to=tmpdir,overwrite=TRUE)
file.copy(from=paste0(bccadir,bccafile),to=tmpdir,overwrite=TRUE)

gcm.tmp <- paste0(tmpdir,'/',gcmfile)
obs.tmp <- paste0(tmpdir,'/',obsfile)
bcci.tmp <- paste0(tmpdir,'/',bccifile)

ci.netcdf.wrapper(gcm.tmp, obs.tmp, bcci.tmp, varname)
##file.copy(from=bcci.tmp,to=writedir,overwrite=TRUE)
print('BCCI Complete')

print('Beginning BCCI Split')
splits <- rep(1:34,each=15)
splitdir <- paste0(writedir,'BCCI/varname,'_',gcm,'_',run,'_',scenario,'_split15/')
dir.create(splitdir,recursive=TRUE)
dsdir <- paste0(writedir,'raw_downscaled/ds_',varname,'_',gcm,'_',run,'_',scenario,'_split15/')
dir.create(dsdir,recursive=TRUE)

for (i in seq_along(unique(splits))) {
   split.info <- split_apart_bcci_file(varname,gcm,bccifile,tmpdir)
   file.copy(from=paste0(split.info$dir,split.info$file),to=splitdir,overwrite=TRUE)
   print('Beginning QDM and Rank on split file')
   print(split.info$file)   
   bcci.split <- split.info$file

   ##Send to a job template and submit
   submit_one_qdm_rank_job <- function(var.name,gcm,run,scenario,i,
                                       obsfile,bcci.split,bccafile,
                                       obsdir,splitdir,bccadir,dsdir)

}

                      

print('Elapsed time')
print(proc.time() - ptm)

file.remove(gcm.tmp)
file.remove(bcci.tmp)
file.remove(obs.tmp)

