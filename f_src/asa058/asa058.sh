#! /bin/bash
#
gfortran -c -Wall asa058.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa058.o ~/lib/asa058.o
#
echo "Normal end of execution."
