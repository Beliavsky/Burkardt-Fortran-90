#! /bin/bash
#
gfortran -c -Wall caesar.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv caesar.o ~/lib/caesar.o
#
echo "Normal end of execution."
