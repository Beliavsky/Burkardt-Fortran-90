#! /bin/bash
#
gfortran -c -Wall lagrange_interp_2d_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran lagrange_interp_2d_test.o $HOME/lib/lagrange_interp_2d.o \
  $HOME/lib/test_interp_2d.o $HOME/lib/r8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm lagrange_interp_2d_test.o
#
mv a.out lagrange_interp_2d_test
./lagrange_interp_2d_test > lagrange_interp_2d_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm lagrange_interp_2d_test
#
echo "Normal end of execution."
