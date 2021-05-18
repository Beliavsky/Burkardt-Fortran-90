#! /bin/bash
#
gfortran -c -Wall svd_snowfall.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv svd_snowfall.o ~/lib/svd_snowfall.o
#
echo "Normal end of execution."
