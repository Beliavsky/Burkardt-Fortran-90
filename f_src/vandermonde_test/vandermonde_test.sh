#! /bin/bash
#
gfortran -c -Wall vandermonde_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran vandermonde_test.o $HOME/lib/vandermonde.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm vandermonde_test.o
#
mv a.out vandermonde_test
./vandermonde_test > vandermonde_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm vandermonde_test
#
echo "Normal end of execution."
