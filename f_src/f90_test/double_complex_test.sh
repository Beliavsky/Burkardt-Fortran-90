#! /bin/bash
#
gfortran -c -Wall double_complex_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran double_complex_test.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm double_complex_test.o
#
mv a.out double_complex_test
./double_complex_test > double_complex_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm double_complex_test
#
echo "Normal end of execution."
