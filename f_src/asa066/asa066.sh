#! /bin/bash
#
gfortran -c -Wall asa066.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa066.o ~/lib/asa066.o
#
echo "Normal end of execution."
