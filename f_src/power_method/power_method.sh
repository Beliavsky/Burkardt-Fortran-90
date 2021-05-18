#! /bin/bash
#
gfortran -c -Wall power_method.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv power_method.o ~/lib/power_method.o
#
echo "Normal end of execution."
