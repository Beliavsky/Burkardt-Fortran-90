#! /bin/bash
#
gfortran -c -Wall lagrange_approx_1d_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran lagrange_approx_1d_test.o $HOME/lib/lagrange_approx_1d.o \
  $HOME/lib/test_interp_1d.o $HOME/lib/qr_solve.o $HOME/lib/r8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm lagrange_approx_1d_test.o
#
mv a.out lagrange_approx_1d_test
./lagrange_approx_1d_test > lagrange_approx_1d_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm lagrange_approx_1d_test
#
echo "Normal end of execution."
