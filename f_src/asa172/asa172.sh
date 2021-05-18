#! /bin/bash
#
gfortran -c -Wall asa172.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa172.o ~/lib/asa172.o
#
echo "Normal end of execution."
