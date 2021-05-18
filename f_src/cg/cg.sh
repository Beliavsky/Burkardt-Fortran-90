#! /bin/bash
#
gfortran -c -Wall cg.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cg.o ~/lib/cg.o
#
echo "Normal end of execution."
