#! /bin/bash
#
gfortran -c -Wall stroud.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv stroud.o ~/lib/stroud.o
#
echo "Normal end of execution."
