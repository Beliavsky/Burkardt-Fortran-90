#! /bin/bash
#
gfortran -c -Wall cvtp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cvtp.o ~/lib/cvtp.o
#
echo "Normal end of execution."
