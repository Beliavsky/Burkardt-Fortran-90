#! /bin/bash
#
gfortran -c -Wall van_der_corput.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv van_der_corput.o ~/lib/van_der_corput.o
#
echo "Normal end of execution."
