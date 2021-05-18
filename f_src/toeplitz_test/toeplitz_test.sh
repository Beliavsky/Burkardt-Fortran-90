#! /bin/bash
#
gfortran -c -Wall toeplitz_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran toeplitz_test.o $HOME/lib/toeplitz.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm toeplitz_test.o
#
mv a.out toeplitz_test
./toeplitz_test > toeplitz_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm toeplitz_test
#
echo "Normal end of execution."
