#! /bin/bash
#
gfortran -c -Wall test_optimization_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran test_optimization_test.o $HOME/lib/test_optimization.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_optimization_test.o
#
mv a.out test_optimization_test
./test_optimization_test > test_optimization_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_optimization_test
#
echo "Normal end of execution."
