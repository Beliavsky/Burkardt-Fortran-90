#! /bin/bash
#
gfortran -c -Wall hermite_cubic.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hermite_cubic.o ~/lib/hermite_cubic.o
#
echo "Normal end of execution."
