#!/bin/bash

module list > /dev/null 2> /dev/null

if [ $? -ne 0 ] ; then
  echo On a system without the module software
  export BASE=${BASE:-/u/Robert.Grumbine/save/}
  export MMAB_VER=v3.4.3
else
#on a system with module software, such as wcoss
  module purge
  #module load ./vessel_icing.modulefile
  #Phase3
  module load EnvVars/1.0.2 ips/18.0.1.163
  module load w3nco/2.0.6 impi/18.0.1 w3emc/2.3.0
  module load bufr/11.2.0 bacio/2.0.2
  echo in makeall.sh loaded modules:
  module list
fi

# for cray: export FC=ftn
export FC=ifort

for d in mmab_gblsice.fd mmab_aksice.fd
do
  cd $d
  make
  cd ..
done
mv mmab_aksice.fd/omb_aksice mmab_gblsice.fd/omb_gblsice ../exec

