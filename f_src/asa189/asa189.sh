#! /bin/bash
#
gfortran -c -Wall asa189.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa189.o ~/lib/asa189.o
#
echo "Normal end of execution."
