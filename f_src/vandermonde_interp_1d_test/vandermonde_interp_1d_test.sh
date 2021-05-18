#! /bin/bash
#
gfortran -c -Wall vandermonde_interp_1d_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran vandermonde_interp_1d_test.o $HOME/lib/vandermonde_interp_1d.o \
  $HOME/lib/qr_solve.o $HOME/lib/condition.o $HOME/lib/test_interp.o \
  $HOME/lib/r8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm vandermonde_interp_1d_test.o
#
mv a.out vandermonde_interp_1d_test
./vandermonde_interp_1d_test > vandermonde_interp_1d_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm vandermonde_interp_1d_test
#
echo "Normal end of execution."
