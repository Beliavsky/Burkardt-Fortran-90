#! /bin/bash
#
gfortran -c -Wall toms672.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv toms672.o ~/lib/toms672.o
#
echo "Normal end of execution."
