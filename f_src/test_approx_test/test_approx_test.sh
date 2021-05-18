#! /bin/bash
#
gfortran -c test_approx_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran test_approx_test.o $HOME/lib/test_approx.o $HOME/lib/spline.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_approx_test.o
#
mv a.out test_approx_test
./test_approx_test > test_approx_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_approx_test
#
echo "Normal end of execution."
