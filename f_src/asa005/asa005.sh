#! /bin/bash
#
gfortran -c -Wall asa005.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa005.o ~/lib/asa005.o
#
echo "Normal end of execution."
