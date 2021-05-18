#! /bin/bash
#
gfortran -c -Wall shepard_interp_nd.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv shepard_interp_nd.o ~/lib/shepard_interp_nd.o
#
echo "Normal end of execution."
