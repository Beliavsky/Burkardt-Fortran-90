#! /bin/bash
#
gfortran -c -Wall chebyshev_series.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv chebyshev_series.o ~/lib/chebyshev_series.o
#
echo "Normal end of execution."
