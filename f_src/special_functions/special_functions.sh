#! /bin/bash
#
gfortran -c -Wall special_functions.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv special_functions.o ~/lib/special_functions.o
#
echo "Normal end of execution."
