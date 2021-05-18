#! /bin/bash
#
gfortran -c -Wall fem_io.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fem_io.o ~/lib/fem_io.o
#
echo "Normal end of execution."
