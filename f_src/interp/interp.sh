#! /bin/bash
#
gfortran -c -Wall interp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv interp.o ~/lib/interp.o
#
echo "Normal end of execution."
