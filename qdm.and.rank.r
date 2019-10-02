##Run CI.R from hClimDown and divide up the BCCI output file into
##10 cell latitude bands

library(hClimDown)

##--------------------------------------------------------------

print('QDM and Rank Started')

ptm <- proc.time()

args <- commandArgs(trailingOnly=TRUE)
for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
}

options(max.GB=1)
options(calibration.start=as.POSIXct('1950-01-01', tz='GMT'))
options(calibration.end=as.POSIXct('2014-12-31', tz='GMT'))

tmpdir <- paste0(tmpdir,'/qdrank_',gcm,'_',varname,'_',run,'_',scenario,'_tmp')

if (!file.exists(tmpdir)) {
   dir.create(tmpdir,recursive=TRUE)
}

##Move files to local storage
print(obsfile)
print(bccidir)
print(bccifile)
file.copy(from=paste0(obsdir,obsfile),to=tmpdir,overwrite=TRUE)
file.copy(from=paste0(bccidir,bccifile),to=tmpdir,overwrite=TRUE)
file.copy(from=paste0(bccadir,bccafile),to=tmpdir,overwrite=TRUE)

qdm.file <- gsub('BCCI','QDM',bccifile)
rank.file <- gsub('BCCI','BCCAQv2',bccifile)

obs.tmp <- paste0(tmpdir,'/',obsfile)
bcci.tmp <- paste0(tmpdir,'/',bccifile)
bcca.tmp <- paste0(tmpdir,'/',bccafile)
qdm.tmp <-  paste0(tmpdir,'/',qdm.file)
rank.tmp <- paste0(tmpdir,'/',rank.file)

print('QDM Stage')
qdm.netcdf.wrapper(obs.tmp, bcci.tmp, qdm.tmp, varname)
##file.copy(from=qdm.tmp,to=writedir,overwrite=TRUE)
print('QDM Complete')

print('Rerank Stage')
load(bcca.tmp)
rerank.netcdf.wrapper(qdm.tmp, obs.tmp, analogues, rank.tmp, varname)

file.copy(from=rank.tmp,to=writedir,overwrite=TRUE)

file.remove(bcci.tmp)
file.remove(obs.tmp)
file.remove(bcca.tmp)
file.remove(qdm.tmp)
file.remove(rank.tmp)

print('Elapsed time')
print(proc.time() - ptm)

