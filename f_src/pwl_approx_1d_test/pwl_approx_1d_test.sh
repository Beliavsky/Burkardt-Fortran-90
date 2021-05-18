#! /bin/bash
#
gfortran -c -Wall pwl_approx_1d_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran pwl_approx_1d_test.o $HOME/lib/pwl_approx_1d.o \
 $HOME/lib/test_interp_1d.o $HOME/lib/qr_solve.o $HOME/lib/r8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm pwl_approx_1d_test.o
#
mv a.out pwl_approx_1d_test
./pwl_approx_1d_test > pwl_approx_1d_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm pwl_approx_1d_test
#
echo "Normal end of execution."
