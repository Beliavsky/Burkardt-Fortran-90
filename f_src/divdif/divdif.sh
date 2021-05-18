#! /bin/bash
#
gfortran -c -Wall divdif.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv divdif.o ~/lib/divdif.o
#
echo "Normal end of execution."
