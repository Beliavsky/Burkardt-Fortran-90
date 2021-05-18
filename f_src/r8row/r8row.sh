#! /bin/bash
#
gfortran -c -Wall r8row.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv r8row.o ~/lib/r8row.o
#
echo "Normal end of execution."
