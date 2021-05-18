#! /bin/bash
#
gfortran -c -Wall test_interp_nd.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_interp_nd.o ~/lib/test_interp_nd.o
#
echo "Normal end of execution."
