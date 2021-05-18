#! /bin/bash
#
gfortran -c -Wall sparse_interp_nd.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv sparse_interp_nd.o ~/lib/sparse_interp_nd.o
#
echo "Normal end of execution."
