#! /bin/bash
#
gfortran -c -Wall laguerre_integrands.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv laguerre_integrands.o ~/lib/laguerre_integrands.o
#
echo "Normal end of execution."
