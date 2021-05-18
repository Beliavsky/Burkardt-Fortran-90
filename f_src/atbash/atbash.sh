#! /bin/bash
#
gfortran -c -Wall atbash.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv atbash.o ~/lib/atbash.o
#
echo "Normal end of execution."
