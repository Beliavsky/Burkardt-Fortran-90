#! /bin/bash
#
gfortran -c -Wall chebyshev_interp_1d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv chebyshev_interp_1d.o ~/lib/chebyshev_interp_1d.o
#
echo "Normal end of execution."
