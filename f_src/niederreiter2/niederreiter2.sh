#! /bin/bash
#
gfortran -c -Wall niederreiter2.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv niederreiter2.o ~/lib/niederreiter2.o
#
echo "Normal end of execution."
