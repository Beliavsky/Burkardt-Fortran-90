#! /bin/bash
#
gfortran -c -Wall cauchy_principal_value.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cauchy_principal_value.o ~/lib/cauchy_principal_value.o
#
echo "Normal end of execution."
