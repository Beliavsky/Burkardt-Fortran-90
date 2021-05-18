#! /bin/bash
#
gfortran -c -Wall matrix_exponential_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran matrix_exponential_test.o $HOME/lib/matrix_exponential.o \
  $HOME/lib/test_matrix_exponential.o $HOME/lib/r8lib.o $HOME/lib/c8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm matrix_exponential_test.o
#
mv a.out matrix_exponential_test
./matrix_exponential_test > matrix_exponential_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm matrix_exponential_test
#
echo "Normal end of execution."
