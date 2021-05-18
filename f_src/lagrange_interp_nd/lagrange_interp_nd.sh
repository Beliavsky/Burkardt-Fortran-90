#! /bin/bash
#
gfortran -c -Wall lagrange_interp_nd.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv lagrange_interp_nd.o ~/lib/lagrange_interp_nd.o
#
echo "Normal end of execution."
