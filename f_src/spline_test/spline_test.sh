#! /bin/bash
#
gfortran -c -Wall spline_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran spline_test.o $HOME/lib/spline.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm spline_test.o
#
mv a.out spline_test
./spline_test > spline_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm spline_test
#
echo "Normal end of execution."
