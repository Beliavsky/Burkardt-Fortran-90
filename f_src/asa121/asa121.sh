#! /bin/bash
#
gfortran -c -Wall asa121.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa121.o ~/lib/asa121.o
#
echo "Normal end of execution."
