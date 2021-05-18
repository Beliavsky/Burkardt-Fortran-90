#! /bin/bash
#
gfortran -c -Wall asa239.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa239.o ~/lib/asa239.o
#
echo "Normal end of execution."
