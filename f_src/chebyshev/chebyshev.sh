#! /bin/bash
#
gfortran -c -Wall chebyshev.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv chebyshev.o ~/lib/chebyshev.o
#
echo "Normal end of execution."
