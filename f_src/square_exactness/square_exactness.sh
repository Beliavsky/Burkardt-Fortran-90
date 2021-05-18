#! /bin/bash
#
gfortran -c -Wall square_exactness.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv square_exactness.o ~/lib/square_exactness.o
#
echo "Normal end of execution."
