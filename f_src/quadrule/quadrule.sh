#! /bin/bash
#
gfortran -c -Wall quadrule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv quadrule.o ~/lib/quadrule.o
#
echo "Normal end of execution."
