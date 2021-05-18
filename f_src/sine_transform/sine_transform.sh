#! /bin/bash
#
gfortran -c -Wall sine_transform.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv sine_transform.o ~/lib/sine_transform.o
#
echo "Normal end of execution."
