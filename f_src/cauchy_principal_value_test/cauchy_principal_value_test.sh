#! /bin/bash
#
gfortran -c -Wall cauchy_principal_value_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cauchy_principal_value_test cauchy_principal_value_test.o $HOME/lib/cauchy_principal_value.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cauchy_principal_value_test.o
#
./cauchy_principal_value_test > cauchy_principal_value_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cauchy_principal_value_test
#
echo "Normal end of execution."
