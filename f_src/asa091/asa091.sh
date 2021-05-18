#! /bin/bash
#
gfortran -c -Wall asa091.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa091.o ~/lib/asa091.o
#
echo "Normal end of execution."
