#!/bin/sh --login
#Robert Grumbine
#variously through 10 September 2018

set -e

module purge
module load prod_envir/1.0.2

# Phase 3
module load EnvVars/1.0.2 ips/18.0.1.163
module load grib_util/1.1.0
module load prod_util/1.1.0 
module load util_shared/1.1.0 #a guess
module load w3nco/2.0.6 impi/18.0.1 w3emc/2.3.0
module load bufr/11.2.0 bacio/2.0.2
# -- to check on a module's usage: module spider $m 

# Show what happened:
module list

export HOMEpmb=/gpfs/tp2/nco/ops/nwprod/util

echo comroot $COMROOT comrot $COMROT

export KEEPDATA="YES"

set -x

tag=`date +"%Y%m%d"`
tagm=`finddate.sh $tag d-1`

  export PDY=$tag
  export PDYm1=$tagm

  for cyc in 00 
  do
    export cyc
    if [ -d $COMROOThps/gfs/prod/gfs.$tag ] ; then
      time ./sms.fake > sms.t${cyc}.$tag
      if [ $cyc -ne 00 ] ; then
        time ./fv3       > fv3.${cyc}.$tag
      fi
    else
      echo could not find $COMROOThps/gfs/prod/gfs.$tag
      ls -lsd $COMROOThps/gfs/prod
      ls -lsd $COMROOThps/gfs/
      ls -lsd $COMROOThps
      exit 1
    fi
  done
