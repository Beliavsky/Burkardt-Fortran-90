#! /bin/bash
#
gfortran -c -Wall pgma_io.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv pgma_io.o ~/lib/pgma_io.o
#
echo "Normal end of execution."
