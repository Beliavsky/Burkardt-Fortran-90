#! /bin/bash
#
gfortran -c -Wall rbf_interp_2d_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran rbf_interp_2d_test.o $HOME/lib/rbf_interp_2d.o \
 $HOME/lib/test_interp_2d.o $HOME/lib/r8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm rbf_interp_2d_test.o
#
mv a.out rbf_interp_2d_test
./rbf_interp_2d_test > rbf_interp_2d_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm rbf_interp_2d_test
#
echo "Normal end of execution."
