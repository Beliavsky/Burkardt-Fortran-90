#! /bin/bash
#
gfortran -c -Wall cvt.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cvt.o ~/lib/cvt.o
#
echo "Normal end of execution."
