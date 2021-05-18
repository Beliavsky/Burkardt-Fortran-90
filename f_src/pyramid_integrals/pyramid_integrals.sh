#! /bin/bash
#
gfortran -c -Wall pyramid_integrals.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv pyramid_integrals.o ~/lib/pyramid_integrals.o
#
echo "Normal end of execution."
