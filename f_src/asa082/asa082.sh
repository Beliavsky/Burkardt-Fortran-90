#! /bin/bash
#
gfortran -c -Wall asa082.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa082.o ~/lib/asa082.o
#
echo "Normal end of execution."
