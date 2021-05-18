#! /bin/bash
#
gfortran -c -Wall rbf_interp_2d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv rbf_interp_2d.o ~/lib/rbf_interp_2d.o
#
echo "Normal end of execution."
