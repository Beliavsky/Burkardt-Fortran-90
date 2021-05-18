#! /bin/bash
#
gfortran -c -Wall newton_interp_1d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv newton_interp_1d.o ~/lib/newton_interp_1d.o
#
echo "Normal end of execution."
