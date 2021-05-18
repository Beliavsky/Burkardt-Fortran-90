#! /bin/bash
#
gfortran -c -Wall pwl_interp_2d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv pwl_interp_2d.o ~/lib/pwl_interp_2d.o
#
echo "Normal end of execution."
