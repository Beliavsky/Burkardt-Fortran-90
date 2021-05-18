#! /bin/bash
#
gfortran -c -Wall nearest_interp_1d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv nearest_interp_1d.o ~/lib/nearest_interp_1d.o
#
echo "Normal end of execution."
