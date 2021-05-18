#! /bin/bash
#
gfortran -c -Wall fem2d_bvp_quadratic_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fem2d_bvp_quadratic_test.o $HOME/lib/fem2d_bvp_quadratic.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fem2d_bvp_quadratic_test.o
#
mv a.out fem2d_bvp_quadratic_test
./fem2d_bvp_quadratic_test > fem2d_bvp_quadratic_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fem2d_bvp_quadratic_test
#
echo "Normal end of execution."
