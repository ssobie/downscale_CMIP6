##BCCA Script

ptm <- proc.time()

library(hClimDown)
library(doParallel)

options(max.GB=1)
options(calibration.start=as.POSIXct('1951-01-01', tz='GMT'))
options(calibration.end=as.POSIXct('2014-12-31', tz='GMT'))

registerDoParallel(cores=4) # or some other number that you're comfortable with.

args <- commandArgs(trailingOnly=TRUE)
for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
}

tmpdir <- paste0(tmpdir,'/bcca_',gcm,'_',varname,'_tmp')
if (!file.exists(tmpdir)) {
   dir.create(tmpdir,recursive=TRUE)
}

##Move files to local storage
file.copy(from=paste0(gcmdir,gcmfile),to=tmpdir,overwrite=TRUE)
file.copy(from=paste0(obsdir,obsfile),to=tmpdir,overwrite=TRUE)

gcm.tmp <- paste0(tmpdir,'/',gcmfile)
obs.tmp <- paste0(tmpdir,'/',obsfile)
bcca.tmp <- paste0(tmpdir,'/',bccafile)

analogues <- ca.netcdf.wrapper(gcm.tmp, obs.tmp, varname)
save(analogues,file=bcca.tmp)

file.copy(from=bcca.tmp,to=writedir,overwrite=TRUE)

Sys.sleep(3)

file.remove(obs.tmp)
file.remove(bcca.tmp)
file.remove(gcm.tmp)

print('Elapsed time')
print(proc.time() - ptm)

