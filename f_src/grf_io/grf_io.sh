#! /bin/bash
#
gfortran -c -Wall grf_io.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv grf_io.o ~/lib/grf_io.o
#
echo "Normal end of execution."
