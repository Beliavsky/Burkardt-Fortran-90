#! /bin/bash
#
gfortran -c -Wall vandermonde_approx_2d_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran vandermonde_approx_2d_test.o $HOME/lib/vandermonde_approx_2d.o \
  $HOME/lib/test_interp_2d.o $HOME/lib/qr_solve.o $HOME/lib/r8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm vandermonde_approx_2d_test.o
#
mv a.out vandermonde_approx_2d_test
./vandermonde_approx_2d_test > vandermonde_approx_2d_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm vandermonde_approx_2d_test
#
echo "Normal end of execution."
