#! /bin/bash
#
gfortran -c -Wall simplex_integrals.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv simplex_integrals.o ~/lib/simplex_integrals.o
#
echo "Normal end of execution."
