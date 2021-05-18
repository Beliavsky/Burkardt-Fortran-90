#! /bin/bash
#
gfortran -c -Wall line_integrals.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv line_integrals.o ~/lib/line_integrals.o
#
echo "Normal end of execution."
