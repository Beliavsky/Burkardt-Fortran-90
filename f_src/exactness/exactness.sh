#! /bin/bash
#
gfortran -c -Wall exactness.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv exactness.o ~/lib/exactness.o
#
echo "Normal end of execution."
