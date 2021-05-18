#! /bin/bash
#
gfortran -c -Wall r8col.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv r8col.o ~/lib/r8col.o
#
echo "Normal end of execution."
