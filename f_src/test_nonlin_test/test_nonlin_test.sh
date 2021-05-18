#! /bin/bash
#
gfortran -c -Wall test_nonlin_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran test_nonlin_test.o $HOME/lib/test_nonlin.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_nonlin_test.o
#
mv a.out test_nonlin_test
./test_nonlin_test > test_nonlin_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_nonlin_test
#
echo "Normal end of execution."
