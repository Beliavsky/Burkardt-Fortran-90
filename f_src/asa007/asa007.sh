#! /bin/bash
#
gfortran -c -Wall asa007.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa007.o ~/lib/asa007.o
#
echo "Normal end of execution."
