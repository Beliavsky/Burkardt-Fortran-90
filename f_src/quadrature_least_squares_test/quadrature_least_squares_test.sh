#! /bin/bash
#
gfortran -c -Wall quadrature_least_squares_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran quadrature_least_squares_test.o $HOME/lib/quadrature_least_squares.o \
  $HOME/lib/qr_solve.o $HOME/lib/r8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm quadrature_least_squares_test.o
#
mv a.out quadrature_least_squares_test
./quadrature_least_squares_test > quadrature_least_squares_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm quadrature_least_squares_test
#
echo "Normal end of execution."
