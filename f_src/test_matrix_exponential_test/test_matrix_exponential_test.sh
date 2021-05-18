#! /bin/bash
#
gfortran -c -Wall test_matrix_exponential_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran test_matrix_exponential_test.o $HOME/lib/test_matrix_exponential.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_matrix_exponential_test.o
#
mv a.out test_matrix_exponential_test
./test_matrix_exponential_test > test_matrix_exponential_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_matrix_exponential_test
#
echo "Normal end of execution."
