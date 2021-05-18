#! /bin/bash
#
gfortran -c -Wall upc.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv upc.o ~/lib/upc.o
#
echo "Normal end of execution."
