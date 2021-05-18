#! /bin/bash
#
gfortran -c -Wall square_integrals_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran square_integrals_test.o $HOME/lib/square_integrals.o
if [ $? -ne 0 ]; then
  echo "Load error"
  exit
fi
rm square_integrals_test.o
#
mv a.out square_integrals_test
./square_integrals_test > square_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm square_integrals_test
#
echo "Normal end of execution."
