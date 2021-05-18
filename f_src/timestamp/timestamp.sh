#! /bin/bash
#
gfortran -c -Wall timestamp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv timestamp.o ~/lib/timestamp.o
#
echo "Normal end of execution."
