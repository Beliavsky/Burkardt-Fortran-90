#! /bin/bash
#
gfortran -c -Wall beta_nc.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv beta_nc.o ~/lib/beta_nc.o
#
echo "Normal end of execution."
