#! /bin/bash
#
gfortran -c -Wall wtime.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv wtime.o ~/lib/wtime.o
#
echo "Normal end of execution."
