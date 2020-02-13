#!/bin/bash                                                                                                                                         
gcm="CanESM5"
run='r8i1p2f1'
scenario='ssp126'

qsub -N "clim.su" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual',climname="su" run.climdex.variables.pbs
qsub -N "clim.su30" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual',climname="su30" run.climdex.variables.pbs
qsub -N "clim.id" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual',climname="id" run.climdex.variables.pbs
qsub -N "clim.fd" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual',climname="fd" run.climdex.variables.pbs
qsub -N "clim.tr" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual',climname="tr" run.climdex.variables.pbs

qsub -N "clim.r10mm" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual',climname="r10mm" run.climdex.variables.pbs
qsub -N "clim.r20mm" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual',climname="r20mm" run.climdex.variables.pbs
qsub -N "clim.sdii" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual',climname="sdii" run.climdex.variables.pbs
qsub -N "clim.prcp" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual',climname="prcptot" run.climdex.variables.pbs
qsub -N "clim.cwd" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual',climname="cwd" run.climdex.variables.pbs
qsub -N "clim.cdd" -v gcm=$gcm,run=$run,scenario=$scenario,type='annual',climname="cdd" run.climdex.variables.pbs

qsub -N "clim.rx1day" -v gcm=$gcm,run=$run,scenario=$scenario,type='monthly',climname="rx1day" run.climdex.variables.pbs
qsub -N "clim.rx2day" -v gcm=$gcm,run=$run,scenario=$scenario,type='monthly',climname="rx2day" run.climdex.variables.pbs
qsub -N "clim.rx5day" -v gcm=$gcm,run=$run,scenario=$scenario,type='monthly',climname="rx5day" run.climdex.variables.pbs

qsub -N "clim.txx" -v gcm=$gcm,run=$run,scenario=$scenario,type='monthly',climname="txx" run.climdex.variables.pbs
qsub -N "clim.txn" -v gcm=$gcm,run=$run,scenario=$scenario,type='monthly',climname="txn" run.climdex.variables.pbs
qsub -N "clim.tnn" -v gcm=$gcm,run=$run,scenario=$scenario,type='monthly',climname="tnn" run.climdex.variables.pbs
qsub -N "clim.tnx" -v gcm=$gcm,run=$run,scenario=$scenario,type='monthly',climname="tnx" run.climdex.variables.pbs

qsub -N "clim.r95" -v gcm=$gcm,run=$run,scenario=$scenario,type='r9',climname="r95" run.climdex.variables.pbs
qsub -N "clim.r99" -v gcm=$gcm,run=$run,scenario=$scenario,type='r9',climname="r99" run.climdex.variables.pbs

qsub -N "dd" -v gcm=$gcm,run=$run,scenario=$scenario,type='degree_days' run.degree.day.variables.pbs
qsub -N "clim.gsl" -v gcm=$gcm,run=$run,scenario=$scenario,type='gsl' run.degree.day.variables.pbs
qsub -N "clim.dtr" -v gcm=$gcm,run=$run,scenario=$scenario,type='dtr' run.degree.day.variables.pbs



