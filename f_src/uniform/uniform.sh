#! /bin/bash
#
gfortran -c -Wall uniform.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv uniform.o ~/lib/uniform.o
#
echo "Normal end of execution."
