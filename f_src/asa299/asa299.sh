#! /bin/bash
#
gfortran -c -Wall asa299.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa299.o ~/lib/asa299.o
#
echo "Normal end of execution."
