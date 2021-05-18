#! /bin/bash
#
gfortran -c -Wall besselj.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv besselj.o ~/lib/besselj.o
#
echo "Normal end of execution."
