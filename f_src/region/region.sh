#! /bin/bash
#
gfortran -c -Wall region.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv region.o ~/lib/region.o
#
echo "Normal end of execution."
