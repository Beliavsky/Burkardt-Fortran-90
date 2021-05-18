#! /bin/bash
#
gfortran -c -Wall prob.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv prob.o ~/lib/prob.o
#
echo "Normal end of execution."
