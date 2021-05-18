#! /bin/bash
#
gfortran -c -Wall square_integrals.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv square_integrals.o ~/lib/square_integrals.o
#
echo "Normal end of execution."
