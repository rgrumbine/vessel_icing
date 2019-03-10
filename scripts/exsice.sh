#!/bin/sh
# -- 09/08/02 -------- BEGIN EXSICE SCRIPT ----------------------------

set -x
cd $DATA

# ######################################################################
# start SICE processing
# modified: 20030929
# changes: 1) new output file added at 3-h intervals out to 168 hours
#             sice.$cycle.siceg for new web page and NAWIPS
#          2) runs on all 4 model cycles
#          3) runs all year
# modified: 20041027
# changes: 1) re-arranged script
#          2) added step to create northern hemispheric GRIB2 bulletins
#             for AWIPS
#    History: OCT 2005  Lilly -- Converted fax graphics to T4 format
#                                and stopped writing to the stat file.
#    History: JUN 2006  Lilly -- Removed the legacy fax graphics for
#                                Vessel Icing.
#
# ######################################################################
 
# **************************************************
#   PRODUCE VESSEL ICING GUIDANCE
# **************************************************

# Collect the gfs and rtg_sst files which will be used:
for fhr in  00  03  06  09  12  15  18  21  24  27  30  33  36  39  42  45  48 \
            51  54  57  60  63  66  69  72  75  78  81  84  87  90  93  96  99 \
           102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 \
           153 156 159 162 165 168
do
  if [ $fhr -lt 100 ] ; then
    fhr=0$fhr
  fi
  cp $COMINgfs/gfs.${cycle}.pgrb2.1p00.f${fhr} pgrb2f${fhr}
  if [ -f $COMINgfs/gfs.${cycle}.pgrb2.1p00.f${fhr}.idx ] ; then 
     cp $COMINgfs/gfs.${cycle}.pgrb2.1p00.f${fhr}.idx pgrb2if${fhr}
  else
#     ${utilexec}/grbindex pgrbf${fhr} pgrbif${fhr}
     $GRB2INDEX pgrb2f${fhr} pgrb2if${fhr}
     export err=$?; err_chk
  fi
done

cp $COMINsst/sst2dvar_grb .

echo $PDY
echo $cyc
echo $PDYm1
  
echo $PDY > dtg.ft90
echo $cyc > dtg.ft91


# Attach file to the unit numbers:
# New (February 2016) for grib2 -- use wgrib2 to extract the desired fields:
iunit=94
junit=194
for fhr in  00  03  06  09  12  15  18  21  24  27  30  33  36  39  42  45  48 \
           51  54  57  60  63  66  69  72  75  78  81  84  87  90  93  96  99 \
          102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 \
          153 156 159 162 165 168
do
  if [ $fhr -lt 100 ] ; then
    fhr=0$fhr
  fi

  if [ $fhr -eq 0 ] ; then
    for field in LAND:surface ICEC:surface 
    do
      grep $field pgrb2if$fhr | $WGRIB2 -i pgrb2f$fhr -append -order we:ns -bin fort.94
    done
  fi

  iunit=`expr $iunit + 1 `
  junit=`expr $junit + 1 `
  for field in ':TMP:2 m above' 'UGRD:10 m above' 'VGRD:10 m above'
  do
    grep "$field" pgrb2if$fhr | $WGRIB2 -i pgrb2f$fhr -append -order we:ns -bin fort.$iunit
  done

#   export XLFUNIT_${iunit}="pgrb2f$fhr"
#   ln -s pgrb2f$fhr fort.${iunit}
   export XLFUNIT_${junit}="pgrb2if$fhr"
   ln -s pgrb2fi$fhr fort.${junit}
done

export XLFRTEOPTS="unit_vars=yes"
export XLFUNIT_52="siceg"
export XLFUNIT_90="dtg.ft90"
export XLFUNIT_91="dtg.ft91"
export XLFUNIT_92="sst2dvar_grb"
ln -sf siceg           fort.52
ln -sf dtg.ft90        fort.90
ln -sf dtg.ft91        fort.91
ln -sf sst2dvar_grb    fort.92

# Run the program:
export pgm="omb_gblsice"; . prep_step
startmsg
$EXECvessel_icing/omb_gblsice >> $pgmout 2>> errfile
export err=$?; err_chk
echo $err

# ****************************************
# Make grib index of the global ice file 
# ****************************************

export pgm="$GRBINDEX"; . prep_step
startmsg
$GRBINDEX siceg sicegi
export err=$?; err_chk

# ****************************************
# Make Alaska files
# ****************************************

export XLFRTEOPTS="unit_vars=yes"
export XLFUNIT_11="siceg"
export XLFUNIT_12="sicegi"
export XLFUNIT_53="siceg_akw"
export XLFUNIT_90="dtg.ft90"
export XLFUNIT_91="dtg.ft91"
ln -fs siceg        fort.11
ln -fs sicegi       fort.12
ln -fs siceg_akw    fort.53
ln -fs dtg.ft90     fort.90
ln -fs dtg.ft91     fort.91

export pgm="omb_aksice"; . prep_step
startmsg
$EXECvessel_icing/omb_aksice >> $pgmout 2>> errfile
export err=$?; err_chk
echo $err

# ****************************************
# Make grib indices of output files
# ****************************************

export pgm="$GRBINDEX"; . prep_step
startmsg
$GRBINDEX siceg_akw sicegi_akw
export err=$?; err_chk

# *************************************************************
# Make GRIB file with northern hemispheric 1x1 deg lon/lat grid
# *************************************************************

export pgm="$COPYGB"; . prep_step
startmsg
$COPYGB -g 232 -x siceg siceg_1x1.all
export err=$?; err_chk

export pgm="$GRBINDEX"; . prep_step
startmsg
$GRBINDEX siceg_1x1.all sicegi_1x1.all
export err=$?; err_chk

# *************************************************************
# Build GRIB bulletins
# *************************************************************

# *************************************************************
# Make grib2 conversion
# *************************************************************

export pgm="$CNVGRIB"; . prep_step
startmsg
$CNVGRIB -g12 -p40 siceg_1x1.all grib2
export err=$?; err_chk

# *************************************************************
# Make grib2 index
# *************************************************************

export pgm="$GRB2INDEX"; . prep_step
startmsg
$GRB2INDEX grib2 gribi2
export err=$?; err_chk

# *************************************************************
# Create data set to send to TOC/AWIPS
# *************************************************************

export pgm="$TOCGRIB2"; . prep_step

export XLFRTEOPTS="unit_vars=yes"

#export XLFUNIT_11="grib2"
#export XLFUNIT_31="gribi2"
#export XLFUNIT_51="xtrn.awpsice.t${cyc}"
export FORT11="grib2"
export FORT31="gribi2"
export FORT51="xtrn.awpsice.t${cyc}"
ln -fs grib2                  fort.11
ln -fs gribi2                 fort.31
ln -fs xtrn.awpsice.t${cyc}   fort.51
#### Debug
echo tocgrib block
echo parmdir = $PARMvessel_icing
echo parfile = $PARMvessel_icing/grib2_awpnsice.232
ls -l $PARMvessel_icing/grib2_awpnsice.232
echo tocgrib2: $TOCGRIB2 
echo end tocgrib block
######
startmsg
$TOCGRIB2 <$PARMvessel_icing/grib2_awpnsice.232 1>> $pgmout 2>> errfile
export err=$?; err_chk

if test $SENDCOM = 'YES'
then
   echo "wcoss" >   $COMOUT/where_sice_ran.$cycle
   cp siceg         $COMOUT/sice.$cycle.siceg
   cp siceg_1x1.all $COMOUT/sice.$cycle.siceg_1x1.all
   cp siceg_akw     $COMOUT/sice.$cycle.siceg_akw

   cp sicegi         $COMOUT/sice.$cycle.sicegi
   cp sicegi_1x1.all $COMOUT/sice.$cycle.sicegi_1x1.all
   cp sicegi_akw     $COMOUT/sice.$cycle.sicegi_akw

   cp xtrn.awpsice.t${cyc} $PCOM/xtrn.awpsice.t${cyc} 

fi

#################################
# Convert to grib2 format
#################################
for fil in siceg siceg_1x1.all siceg_akw
do
  $CNVGRIB -g12 -p40 $COMOUT/sice.$cycle.${fil} $COMOUT/sice.$cycle.${fil}.grib2
  export err=$?; err_chk
  $WGRIB2 $COMOUT/sice.$cycle.${fil}.grib2 -s > $COMOUT/sice.$cycle.${fil}.grib2.idx
  export err=$?; err_chk
done

# ******************************************
# Send files to ftpprd and pcom with alerts
# ******************************************

if [ "$SENDDBN" = 'YES' ]
then
# add line to send xtrn.awpsice.t${cyc} to pcom and alert the TOC
# JY - turn off the alert -05/11/2016
  #if [ "$SENDDBN_NTC" = 'YES' ]; then
  #  $DBNROOT/bin/dbn_alert GRIB_LOW OMBSICE $job $PCOM/xtrn.awpsice.t${cyc}
  #fi
  $DBNROOT/bin/dbn_alert MODEL OMBSICE $job $COMOUT/sice.$cycle.siceg
  $DBNROOT/bin/dbn_alert MODEL OMBSICE $job $COMOUT/sice.$cycle.siceg_1x1.all
  $DBNROOT/bin/dbn_alert MODEL OMBSICE $job $COMOUT/sice.$cycle.siceg_akw

  $DBNROOT/bin/dbn_alert MODEL OMBSICE_GB2 $job $COMOUT/sice.$cycle.siceg.grib2
  $DBNROOT/bin/dbn_alert MODEL OMBSICE_GB2 $job $COMOUT/sice.$cycle.siceg_1x1.all.grib2
  $DBNROOT/bin/dbn_alert MODEL OMBSICE_GB2 $job $COMOUT/sice.$cycle.siceg_akw.grib2

  $DBNROOT/bin/dbn_alert MODEL OMBSICE_GB2_WIDX $job $COMOUT/sice.$cycle.siceg.grib2.idx
  $DBNROOT/bin/dbn_alert MODEL OMBSICE_GB2_WIDX $job $COMOUT/sice.$cycle.siceg_1x1.all.grib2.idx
  $DBNROOT/bin/dbn_alert MODEL OMBSICE_GB2_WIDX $job $COMOUT/sice.$cycle.siceg_akw.grib2.idx
fi

set +x
echo " ***** PROCESSING COMPLETED NORMALLY *****"

# ---------------END OF EXSICE SCRIPT----------------------------
