#! /bin/bash
#
gfortran -c -Wall asa266.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa266.o ~/lib/asa266.o
#
echo "Normal end of execution."
