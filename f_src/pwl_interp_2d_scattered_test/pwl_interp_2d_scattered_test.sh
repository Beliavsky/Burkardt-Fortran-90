#! /bin/bash
#
gfortran -c -Wall pwl_interp_2d_scattered_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran pwl_interp_2d_scattered_test.o $HOME/lib/pwl_interp_2d_scattered.o \
  $HOME/lib/test_interp_2d.o $HOME/lib/r8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm pwl_interp_2d_scattered_test.o
#
mv a.out pwl_interp_2d_scattered_test
./pwl_interp_2d_scattered_test > pwl_interp_2d_scattered_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm pwl_interp_2d_scattered_test
#
echo "Normal end of execution."
