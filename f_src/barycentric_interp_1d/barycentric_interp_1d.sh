#! /bin/bash
#
gfortran -c -Wall barycentric_interp_1d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv barycentric_interp_1d.o ~/lib/barycentric_interp_1d.o
#
echo "Normal end of execution."
