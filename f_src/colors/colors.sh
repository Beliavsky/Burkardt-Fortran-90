#! /bin/bash
#
gfortran -c -Wall colors.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv colors.o ~/lib/colors.o
#
echo "Normal end of execution."
