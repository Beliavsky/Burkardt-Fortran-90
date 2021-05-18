#! /bin/bash
#
gfortran -c -Wall i4lib.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv i4lib.o ~/lib/i4lib.o
#
echo "Normal end of execution."
