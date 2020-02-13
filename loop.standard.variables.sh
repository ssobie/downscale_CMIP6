#!/bin/bash                                                                                                                                         

gcm="CanESM5"
run='r8i1p2f1'
scenario='ssp126'


qsub -N "${gcm}.an.pr" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual',varname="pr",pctl='000' run.standard.variables.pbs
qsub -N "${gcm}.an.tx" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual',varname="tasmax",pctl='000' run.standard.variables.pbs
qsub -N "${gcm}.qt.tn" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual',varname="tasmin",pctl='000' run.standard.variables.pbs

qsub -N "${gcm}.se.pr" -v gcm=$gcm,run=$run,scenario=$scenario,type='seasonal',varname="pr",pctl='000' run.standard.variables.pbs
qsub -N "${gcm}.se.tx" -v gcm=$gcm,run=$run,scenario=$scenario,type='seasonal',varname="tasmax",pctl='000' run.standard.variables.pbs
qsub -N "${gcm}.se.tn" -v gcm=$gcm,run=$run,scenario=$scenario,type='seasonal',varname="tasmin",pctl='000' run.standard.variables.pbs

qsub -N "${gcm}.mon.pr" -v gcm=$gcm,run=$run,scenario=$scenario,type='monthly',varname="pr",pctl='000' run.standard.variables.pbs
qsub -N "${gcm}.mon.tx" -v gcm=$gcm,run=$run,scenario=$scenario,type='monthly',varname="tasmax",pctl='000' run.standard.variables.pbs
qsub -N "${gcm}.mon.tn" -v gcm=$gcm,run=$run,scenario=$scenario,type='monthly',varname="tasmin",pctl='000' run.standard.variables.pbs

qsub -N "${gcm}.ex.pr" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual_extremes',varname="pr",pctl='000' run.standard.variables.pbs
qsub -N "${gcm}.ex.tx" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual_extremes',varname="tasmax",pctl='000' run.standard.variables.pbs
qsub -N "${gcm}.ex.tn" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual_extremes',varname="tasmin",pctl='000' run.standard.variables.pbs

qsub -N "${gcm}.qt.tx" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual_quantiles',varname="tasmax",pctl="975" run.standard.variables.pbs
qsub -N "${gcm}.qt.tx" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual_quantiles',varname="tasmax",pctl="990" run.standard.variables.pbs
qsub -N "${gcm}.qt.tx" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual_quantiles',varname="tasmax",pctl="996" run.standard.variables.pbs

qsub -N "${gcm}.qt.tn" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual_quantiles',varname="tasmin",pctl="004" run.standard.variables.pbs
qsub -N "${gcm}.qt.tn" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual_quantiles',varname="tasmin",pctl="010" run.standard.variables.pbs
qsub -N "${gcm}.qt.tn" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual_quantiles',varname="tasmin",pctl="025" run.standard.variables.pbs
