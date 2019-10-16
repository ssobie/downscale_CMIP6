#!/bin/bash
#PBS -l walltime=24:00:00
#PBS -l nodes=1:ppn=1
#PBS -l pmem=10000Mb
#PBS -d /storage/home/ssobie/code/repos/downscale_CMIP6/
#PBS -o output=./out_files/comb_var.out
#PBS -e error=./out_files/comb_var.err
#PBS -N jobname


cd /storage/home/ssobie/code/repos/downscale_CMIP6/
echo "Current working directory is `pwd`"

gcm="GCM"
run="run"
scenario="scenario"
varname="var"

R CMD BATCH --no-save --no-restore "--args tmpdir='$TMPDIR' varname='$varname' gcm='$gcm' run='$run' scenario='$scenario'" combine.bccaqv2.raw.into.single.file.r "./out_files/${gcm}.${varname}.${scenario}.${run}.combine.out"
