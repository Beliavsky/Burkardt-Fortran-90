#! /bin/bash
#
gfortran -c -Wall fn.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fn.o ~/lib/fn.o
#
echo "Normal end of execution."
