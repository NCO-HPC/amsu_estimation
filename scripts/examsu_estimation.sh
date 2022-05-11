#!/bin/sh
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         examsu_intensity.sh.sms                                #
# Script description:                                                         # 
#                                                                             #
# Author:        Julia Zhu         Org: NP11         Date: 2005-07-12         #
#                                                                             #
# Abstract: This script is the driver script for the STORM Intensity          #
#           estimation using AMSU algorithm                                   # 
#                                                                             # 
# Sub-scripts called:                                                         #
#    amsu_convertgfs.pl:  Convert the GFS files                               #
#    dumpjb : Get the AMSU satellite data                                     #
#    amsu_estimates.pl: Intensity estimation                                  #
#                                                                             #
# Script history log:                                                         #
# 2005-07-14  Julia Zhu                                                       #
#                                                                             #
###############################################################################
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation

set -x

cd $DATA

msg="$job has begun on `hostname`"
postmsg "$jlogfile" "$msg"

#############################
# 1 convert gfs files
#############################
$USHnhc/amsu_convertgfs.pl
if [ $? -ne 1 ]; then
    echo "WARNING: GFS file conversion may not have completed successfully!"
fi

#########################################
# 2 run dumpjb to get the satellite data
#########################################
export cdate=$PDY$cyc
export RADIUS=6.0
export TYPE=amuata

#/nwprod/ush/dumpjb $cdate $RADIUS $TYPE

####################################################
# 3 storm intensity estimation using AMSU algorithm
####################################################
$USHnhc/amsu_estimates.pl -date $PDY$cyc
if [ $? -ne 0 ]; then
    echo "WARNING: Three-storm intensity estimation may not have completed successfully!"
fi

msg='THE AMSU_INTENSITY JOB HAS ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
