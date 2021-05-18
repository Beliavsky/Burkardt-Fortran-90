#! /bin/bash
#
gfortran -c -Wall test_zero_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran test_zero_test.o $HOME/lib/test_zero.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_zero_test.o
#
mv a.out test_zero_test
./test_zero_test > test_zero_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_zero_test
#
echo "Normal end of execution."
