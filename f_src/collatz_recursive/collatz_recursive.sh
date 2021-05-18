#! /bin/bash
#
gfortran -c -Wall collatz_recursive.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv collatz_recursive.o ~/lib/collatz_recursive.o
#
echo "Normal end of execution."
