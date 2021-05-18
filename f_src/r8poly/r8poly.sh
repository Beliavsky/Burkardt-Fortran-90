#! /bin/bash
#
gfortran -c -Wall r8poly.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv r8poly.o ~/lib
#
echo "Normal end of execution."
