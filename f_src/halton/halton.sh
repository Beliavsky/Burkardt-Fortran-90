#! /bin/bash
#
gfortran -c -Wall halton.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv halton.o ~/lib/halton.o
#
echo "Normal end of execution."
