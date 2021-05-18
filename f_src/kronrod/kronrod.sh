#! /bin/bash
#
gfortran -c -Wall kronrod.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv kronrod.o ~/lib/kronrod.o
#
echo "Normal end of execution."
