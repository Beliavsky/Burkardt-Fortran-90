#! /bin/bash
#
gfortran -c -Wall hermite_interpolant_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran hermite_interpolant_test.o /$HOME/lib/hermite_interpolant.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hermite_interpolant_test.o
#
mv a.out hermite_interpolant_test
./hermite_interpolant_test > hermite_interpolant_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hermite_interpolant_test
#
echo "Normal end of execution."
