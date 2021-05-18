#! /bin/bash
#
gfortran -c -Wall hankel_spd.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hankel_spd.o ~/lib/hankel_spd.o
#
echo "Normal end of execution."
