#! /bin/bash
#
gfortran -c -Wall hilbert_curve_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o hilbert_curve_test hilbert_curve_test.o $HOME/lib/hilbert_curve.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hilbert_curve_test.o
#
./hilbert_curve_test > hilbert_curve_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hilbert_curve_test
#
echo "Normal end of execution."
