#! /bin/bash
#
gfortran -c -Wall polynomials_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o polynomials_test polynomials_test.o $HOME/lib/polynomials.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm polynomials_test.o
#
./polynomials_test > polynomials_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm polynomials_test
#
echo "Normal end of execution."
