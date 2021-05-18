#! /bin/bash
#
gfortran -c -Wall asa076.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa076.o ~/lib/asa076.o
#
echo "Normal end of execution."
