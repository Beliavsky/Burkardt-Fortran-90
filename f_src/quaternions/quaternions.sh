#! /bin/bash
#
gfortran -c -Wall quaternions.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv quaternions.o ~/lib/quaternions.o
#
echo "Normal end of execution."
