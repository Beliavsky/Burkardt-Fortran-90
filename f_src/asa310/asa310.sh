#! /bin/bash
#
gfortran -c -Wall asa310.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa310.o ~/lib/asa310.o
#
echo "Normal end of execution."
