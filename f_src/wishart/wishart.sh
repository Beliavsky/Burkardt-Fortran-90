#! /bin/bash
#
gfortran -c -Wall wishart.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv wishart.o ~/lib/wishart.o
#
echo "Normal end of execution."
