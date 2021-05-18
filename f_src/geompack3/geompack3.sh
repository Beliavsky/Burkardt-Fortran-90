#! /bin/bash
#
gfortran -c -Wall geompack3.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv geompack3.o ~/lib/geompack3.o
#
echo "Normal end of execution."
