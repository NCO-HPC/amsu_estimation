#!/bin/bash

set -x

AMSUsorc=`pwd`

source ../versions/build.ver
module reset
module use `pwd`
module load build-amsu_estimation.module.lua
module list

cd $AMSUsorc/../lib/sorc/gfstopack
make clean
make
make install

cd $AMSUsorc
if [ ! -d $AMSUsorc/../exec ]; then
  mkdir -m 775 $AMSUsorc/../exec
fi
for dir in `ls -d *.fd`
do
  cd $AMSUsorc/$dir
  make clean
  make
  make install
done
